# search_ris_wos(): 本地 RIS 的 WoS 风格检索，不依赖额外 R 包。
# 支持：单个 TS=(...)、AND/OR/NOT、括号、双引号短语、* ? $、NEAR/n。
# 不复现 WoS 在线词形还原、拼写扩展或未导出的 Keywords Plus。
# 标点和连字符按分词符处理；短语/邻近检索不跨字段或关键词条目。
# 当前环境没有 R，此函数尚未通过 R 运行验证。
#
# source("search_ris_wos.R") 仅加载函数，不自动检索或导出。
# 调用示例：
# x <- search_ris_wos(
#   ris_file = "asreview_relevant_SoilBEF_Synthesis_PaperScreening_26813.ris",
#   query = 'TS=(dilut*)', expected_n = 26813,
#   output_dir = "results_dilution"
# )
# x$summary
# View(x$results)
# 不导出时省略 output_dir。相同 output_dir 的文件会被覆盖。

search_ris_wos <- function(ris_file, query,
                           output_dir = NULL,
                           expected_n = NULL,
                           view = FALSE,
                           verbose = TRUE) {
  if (!is.character(ris_file) || length(ris_file) != 1L || is.na(ris_file))
    stop("ris_file 必须是单个文件路径。")
  if (!is.character(query) || length(query) != 1L || is.na(query) || !nzchar(trimws(query)))
    stop("query 必须是单个非空检索式。")
  if (!is.null(output_dir) && (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)))
    stop("output_dir 必须为 NULL 或单个文件夹路径。")
  if (!file.exists(ris_file)) {
    stop("找不到文件：", ris_file,
         "\n请将 RIS 放在 getwd() 显示的目录，或把 ris_file 改为完整路径。")
  }
  lines <- readLines(ris_file, encoding = "UTF-8", warn = FALSE)
  lines <- sub("^\ufeff", "", lines)
  records <- list(); raw_records <- list(); current <- NULL; last <- NULL
  block <- character()
  for (line in lines) {
    if (grepl("^[A-Z0-9]{2}  -", line)) {
      tag <- substr(line, 1, 2)
      value <- sub("^[A-Z0-9]{2}  - ?", "", line)
      if (tag == "TY") {
        if (!is.null(current)) stop("检测到缺少 ER 的记录，请先检查 RIS。")
        current <- list(); block <- character(); last <- NULL
      }
      if (!is.null(current)) {
        block <- c(block, line)
        if (tag == "ER") {
          records[[length(records) + 1L]] <- current
          raw_records[[length(raw_records) + 1L]] <- block
          current <- NULL; last <- NULL
        } else {
          current[[tag]] <- c(current[[tag]], value); last <- tag
        }
      }
    } else if (!is.null(current) && !is.null(last) && nzchar(trimws(line))) {
      block <- c(block, line)
      n <- length(current[[last]])
      current[[last]][n] <- paste(current[[last]][n], trimws(line))
    }
  }
  if (!is.null(current)) stop("文件末尾缺少 ER，请检查 RIS 完整性。")
  if (!length(records)) stop("未识别到 RIS 记录。")
  field <- function(r, tags, sep = " ") {
    for (tag in tags) {
      v <- r[[tag]]; v <- v[!is.na(v) & nzchar(trimws(v))]
      if (length(v)) return(paste(v, collapse = sep))
    }
    ""
  }
  extract <- function(tags, sep = " ") {
    vapply(records, field, character(1), tags = tags, sep = sep)
  }
  literature <- data.frame(
    record_id = seq_along(records), title = extract(c("TI", "T1")),
    abstract = extract(c("AB", "N2")), keywords = extract("KW", "; "),
    year = extract("PY"), journal = extract(c("JO", "JF", "T2", "JA")),
    doi = extract("DO"), stringsAsFactors = FALSE
  )
  if (!is.null(expected_n) && nrow(literature) != expected_n) {
    warning("记录数与 expected_n 不一致：读取 ", nrow(literature), "，期望 ", expected_n)
  }
  
  # 将标点/连字符作为词的分隔符；各字段独立，短语/NEAR 不跨字段。
  # KW 中每个关键词独立匹配短语/NEAR，避免跨关键词产生伪命中。
  normalize <- function(x) {
    x <- tolower(enc2utf8(x))
    trimws(gsub("[^\\p{L}\\p{N}_]+", " ", x, perl = TRUE))
  }
  fields <- lapply(seq_along(records), function(i) {
    normalize(c(literature$title[i], literature$abstract[i], records[[i]][["KW"]]))
  })
  term_pattern <- function(term) {
    term <- sub('^"', '', sub('"$', '', term))
    chars <- strsplit(tolower(term), "", fixed = TRUE)[[1]]
    pieces <- vapply(chars, function(ch) {
      if (ch == "*") return("[\\p{L}\\p{N}_]*")
      if (ch == "?") return("[\\p{L}\\p{N}_]")
      if (ch == "$") return("[\\p{L}\\p{N}_]?")
      if (grepl("^[\\p{L}\\p{N}_]$", ch, perl = TRUE)) return(ch)
      " "
    }, character(1))
    p <- trimws(gsub(" +", " ", paste0(pieces, collapse = "")))
    if (!nzchar(p)) stop("无有效检索词：", term)
    paste0("(?<![\\p{L}\\p{N}_])", p, "(?![\\p{L}\\p{N}_])")
  }
  
  # 解析 TS=(...)；优先级：NEAR > NOT > AND > OR。
  q <- trimws(sub("^\\s*TS\\s*=\\s*", "", query, ignore.case = TRUE))
  if (grepl("[A-Za-z]{2,3}\\s*=", q)) stop("本脚本仅接受单个 TS=(...) 检索式。")
  if (sum(strsplit(q, "", fixed = TRUE)[[1]] == '"') %% 2L) stop("双引号不配对。")
  tokens <- regmatches(q, gregexpr('"[^"]*"|[()]|[^[:space:]()"]+', q, perl = TRUE))[[1]]
  if (!length(tokens)) stop("检索式为空。")
  is_op <- function(t) toupper(t) %in% c("AND", "OR", "NOT", "SAME") ||
    grepl("^NEAR(?:/[0-9]+)?$", t, ignore.case = TRUE, perl = TRUE)
  if (any(toupper(tokens) == "SAME")) stop("SAME 为地址检索运算符，本 TS 脚本不支持。")
  # 相邻的检索词默认 AND，与 WoS 英文主题检索一致。
  expanded <- character()
  for (t in tokens) {
    if (length(expanded)) {
      prev <- tail(expanded, 1)
      end_prev <- prev == ")" || (!is_op(prev) && prev != "(")
      start_now <- t == "(" || (!is_op(t) && t != ")")
      if (end_prev && start_now) expanded <- c(expanded, "AND")
    }
    expanded <- c(expanded, t)
  }
  tokens <- expanded; pos <- 1L
  peek <- function() if (pos <= length(tokens)) tokens[pos] else ""
  take <- function() { t <- peek(); pos <<- pos + 1L; t }
  precedence <- function(t) {
    t <- toupper(t)
    if (grepl("^NEAR(?:/[0-9]+)?$", t, perl = TRUE)) return(40L)
    switch(t, NOT = 30L, AND = 20L, OR = 10L, -1L)
  }
  parse_expr <- function(min_prec = 0L) {
    t <- take()
    if (!nzchar(t)) stop("检索式缺少检索词。")
    if (t == "(") {
      node <- parse_expr()
      if (take() != ")") stop("括号不配对。")
    } else {
      if (t == ")" || is_op(t)) stop("运算符前缺少检索词：", t)
      node <- list(op = "TERM", text = t)
    }
    while (precedence(peek()) >= min_prec) {
      op <- toupper(take()); rhs <- parse_expr(precedence(op) + 1L)
      node <- list(op = op, left = node, right = rhs)
    }
    node
  }
  tree <- parse_expr()
  if (pos <= length(tokens)) stop("无法解析检索式，位置：", peek())
  
  # NEAR 的两个操作数允许词、短语、OR 或嵌套 NEAR；不接受 AND/NOT。
  check_near <- function(node, inside = FALSE) {
    if (node$op == "TERM") return(invisible(NULL))
    if (inside && node$op %in% c("AND", "NOT"))
      stop("NEAR 操作数内不能包含 AND/NOT，请改写检索式。")
    nested <- inside || grepl("^NEAR", node$op)
    check_near(node$left, nested); check_near(node$right, nested)
  }
  check_near(tree)
  
  empty_spans <- function() matrix(integer(), ncol = 2)
  spans <- function(node, text) {
    if (node$op == "TERM") {
      m <- gregexpr(term_pattern(node$text), text, perl = TRUE)[[1]]
      if (m[1] < 0L) return(empty_spans())
      len <- attr(m, "match.length")
      starts <- vapply(m, function(a) {
        prefix <- if (a > 1L) substr(text, 1L, a - 1L) else ""
        nchar(gsub("[^ ]", "", prefix)) + 1L
      }, integer(1))
      widths <- vapply(seq_along(m), function(j) {
        s <- substr(text, m[j], m[j] + len[j] - 1L)
        nchar(gsub("[^ ]", "", s)) + 1L
      }, integer(1))
      return(cbind(starts, starts + widths - 1L))
    }
    a <- spans(node$left, text); b <- spans(node$right, text)
    if (node$op == "OR") return(rbind(a, b))
    if (!grepl("^NEAR", node$op)) stop("无效 NEAR 操作数。")
    if (!nrow(a) || !nrow(b)) return(empty_spans())
    distance <- if (node$op == "NEAR") 15L else as.integer(sub("NEAR/", "", node$op))
    out <- list()
    for (i in seq_len(nrow(a))) for (j in seq_len(nrow(b))) {
      # 重复词必须出现于不同位置。
      if (identical(node$left, node$right) && all(a[i, ] == b[j, ])) next
      gap <- max(0L, a[i, 1] - b[j, 2] - 1L, b[j, 1] - a[i, 2] - 1L)
      if (gap <= distance) out[[length(out) + 1L]] <- c(min(a[i, 1], b[j, 1]), max(a[i, 2], b[j, 2]))
    }
    if (!length(out)) empty_spans() else unique(do.call(rbind, out))
  }
  cache <- new.env(parent = emptyenv())
  evaluate <- function(node) {
    if (node$op == "TERM") {
      key <- node$text
      if (!exists(key, cache, inherits = FALSE)) {
        pattern <- term_pattern(key)
        assign(key, vapply(fields, function(f) any(grepl(pattern, f, perl = TRUE)), logical(1)), cache)
      }
      return(get(key, cache, inherits = FALSE))
    }
    a <- evaluate(node$left); b <- evaluate(node$right)
    if (node$op == "AND") return(a & b)
    if (node$op == "OR") return(a | b)
    if (node$op == "NOT") return(a & !b)
    hit <- rep(FALSE, length(records))
    for (i in which(a & b)) hit[i] <- any(vapply(fields[[i]], function(f) nrow(spans(node, f)) > 0L, logical(1)))
    hit
  }
  hit <- evaluate(tree)
  results <- literature[hit, , drop = FALSE]
  output_files <- NULL
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    write.csv(results, file.path(output_dir, "search_results.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(as.character(unlist(lapply(which(hit), function(i) c(raw_records[[i]], "")), use.names = FALSE)),
               file.path(output_dir, "search_results.ris"), useBytes = TRUE)
    saveRDS(list(query = query, results = results), file.path(output_dir, "search_results.rds"))
    writeLines(c(paste("Date:", Sys.time()), paste("Source:", normalizePath(ris_file)),
                 paste("Query:", query), paste("Total:", nrow(literature)), paste("Hits:", nrow(results)),
                 "Scope: exported title, abstract and KW; no online lemmatization/spelling expansion.",
                 "Punctuation/hyphens treated as word separators; proximity does not cross fields or keyword entries."),
               file.path(output_dir, "search_log.txt"), useBytes = TRUE)
    output_files <- file.path(normalizePath(output_dir),
                              c("search_results.csv", "search_results.ris",
                                "search_results.rds", "search_log.txt"))
  }
  if (verbose) message("读取：", nrow(literature), " 条；命中：", nrow(results), " 条。")
  if (view && interactive()) View(results)
  return(list(
    query = query,
    summary = data.frame(total = nrow(literature), hits = nrow(results)),
    results = results,
    literature = literature,
    hit = hit,
    output_files = output_files
  ))
}
