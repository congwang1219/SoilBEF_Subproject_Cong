rm(list = ls())

#少了7篇，合并到9033给luise的库中
setwd("D:/idiv2/AP2_meta/5.subproject20261015/Papers from Luise")



read_ris <- function(file, expected) {
  x <- readLines(file, encoding = "UTF-8", warn = FALSE)
  if (length(x)) x[1] <- sub("^\ufeff", "", x[1])
  
  tags <- grep("^TY\\s+-\\s*\\S|^ER\\s+-\\s*$", x, value = TRUE)
  if (!identical(substr(tags, 1, 2), rep(c("TY", "ER"), expected))) {
    stop(file, " 的 RIS 边界或记录数有误，预期 ", expected, " 条。")
  }
  
  message(file, " 检查通过，共 ", expected, " 条。")
  x
}

main <- read_ris("merged_sum_GHG_dedup_9033.ris", 9033L)
extra <- lapply(paste0(1:7, ".ris"), read_ris, expected = 1L)

out <- "merged_sum_GHG_dedup_9040.ris"
if (file.exists(out)) stop("输出文件已存在：", out)

merged <- c(main, "", unlist(lapply(extra, c, ""), use.names = FALSE))
writeLines(enc2utf8(merged), out, useBytes = TRUE)

stopifnot(identical(read_ris(out, 9040L), merged))
#---------------------------------------------------------------------------4255RIS组合

setwd("D:/idiv2/AP2_meta/5.subproject20261015/Papers from Luise")

csv_files <- c(
  "asreview_relevant_allGHGs_noDup.csv",
  "asreview_relevant_unscreened_13.03.26.csv"
)
ris_file <- "merged_sum_GHG_dedup_9040.ris"

# 1. 统计并合并两个 CSV，保留两份文件的所有列
tables <- lapply(csv_files, function(file) {
  x <- read.csv(file, fileEncoding = "UTF-8-BOM",
                check.names = FALSE, colClasses = "character",
                na.strings = "", row.names = NULL)
  names(x)[names(x) == ""] <- "source_row_index"
  stopifnot(all(c("title", "abstract", "doi") %in% names(x)))
  x$source_csv <- file
  x
})
print(data.frame(file = csv_files, records = vapply(tables, nrow, integer(1))),
      row.names = FALSE)

columns <- unique(unlist(lapply(tables, names)))
merged <- do.call(rbind, lapply(tables, function(x) {
  for (name in setdiff(columns, names(x))) x[[name]] <- NA_character_
  x[columns]
}))
message("合并 CSV 共 ", nrow(merged), " 条。")

# 2. 读取完整 RIS 记录，建立 DOI 和标题索引
lines <- readLines(ris_file, encoding = "UTF-8", warn = FALSE)
if (length(lines)) lines[1L] <- sub("^\ufeff", "", lines[1L])
starts <- grep("^TY[ \t]+-", lines)
ends <- grep("^ER[ \t]+-[ \t]*$", lines)
stopifnot(length(starts) == 9040L, length(ends) == 9040L,
          all(starts < ends), all(head(ends, -1L) < tail(starts, -1L)))
records <- Map(function(a, b) lines[a:b], starts, ends)

field <- function(record, tags) {
  pattern <- paste0("^(?:", tags, ")[ \t]+-[ \t]*")
  sub(pattern, "", grep(pattern, record, value = TRUE, perl = TRUE),
      perl = TRUE)
}
norm_text <- function(x) {
  x[is.na(x)] <- ""
  gsub("[^\\p{L}\\p{N}]", "", tolower(enc2utf8(x)), perl = TRUE)
}
norm_doi <- function(x) {
  x[is.na(x)] <- ""
  sub("^(https?://(dx\\.)?doi\\.org/|doi:[ \t]*)", "", tolower(trimws(x)))
}
make_index <- function(keys) {
  ids <- rep(seq_along(keys), lengths(keys))
  values <- unlist(keys, use.names = FALSE)
  keep <- nzchar(values)
  split(ids[keep], values[keep])
}

ris_dois <- lapply(records, function(r) norm_doi(field(r, "DO")))
doi_index <- make_index(ris_dois)
title_index <- make_index(lapply(records, function(r) {
  norm_text(field(r, "TI|T1"))
}))
ris_abstract <- vapply(records, function(r) {
  norm_text(paste(field(r, "AB|N2"), collapse = " "))
}, character(1))

# 3. 先匹配 DOI 和标题。遇到 DOI 差异时，用标题和摘要复核
csv_doi <- norm_doi(merged$doi)
csv_title <- norm_text(merged$title)
csv_abstract <- substr(norm_text(merged$abstract), 1L, 300L)

matched <- vapply(seq_len(nrow(merged)), function(i) {
  d <- unique(doi_index[[csv_doi[i]]])
  t <- unique(title_index[[csv_title[i]]])
  both <- intersect(d, t)
  
  if (length(both) == 1L) return(both)
  if (length(d) == 1L && !length(t)) return(d)
  
  if (!length(d)) {
    compatible <- t[vapply(t, function(j) {
      !nzchar(csv_doi[i]) || !any(nzchar(ris_dois[[j]]))
    }, logical(1))]
    if (length(compatible) == 1L) return(compatible)
  }
  
  if (nchar(csv_abstract[i]) >= 150L) {
    confirmed <- t[startsWith(ris_abstract[t], csv_abstract[i])]
    if (length(confirmed) == 1L) return(confirmed)
  }
  
  NA_integer_
}, integer(1))

merged$source_ris_record <- matched
write.csv(merged, "GHG_ASReview_merged.csv", row.names = FALSE,
          fileEncoding = "UTF-8", na = "")

message("匹配成功 ", sum(!is.na(matched)), " 条，未匹配 ",
        sum(is.na(matched)), " 条。")

if (anyNA(matched)) {
  print(merged[is.na(matched), c("source_csv", "doi", "title")], row.names = FALSE)
  stop("请核查以上记录。")
}
if (anyDuplicated(matched)) {
  repeated <- duplicated(matched) | duplicated(matched, fromLast = TRUE)
  print(merged[repeated, c("source_csv", "doi", "title", "source_ris_record")],
        row.names = FALSE)
  stop("多行 CSV 匹配到同一条 RIS 记录，请核查以上记录。")
}

# 4. 按合并 CSV 的顺序导出原始 RIS 记录
out <- sprintf("GHG_ASReview_matched_%d.ris", length(matched))
writeLines(enc2utf8(unlist(lapply(records[matched], c, ""), use.names = FALSE)),
           out, useBytes = TRUE)
message("导出完成，共 ", length(matched), " 条。文件：", out)


#------------------------------------------------------------------------------double check with GPT

setwd("D:/idiv2/AP2_meta/5.subproject20261015/code/network_agri_pool")

read_ris <- function(file, expected) {
  if (!file.exists(file)) stop("找不到文件：", file)
  
  x <- enc2utf8(readLines(file, encoding = "UTF-8", warn = FALSE))
  if (length(x)) x[1L] <- sub("^\ufeff", "", x[1L])
  
  tags <- grep("^TY[ \t]+-|^ER[ \t]+-[ \t]*$", x, value = TRUE)
  if (!identical(substr(tags, 1L, 2L), rep(c("TY", "ER"), expected))) {
    stop(file, " 的记录数或 TY/ER 边界有误，预期 ", expected, " 条。")
  }
  x
}

soilbef <- read_ris("asreview_relevant_SoilBEF_Synthesis_PaperScreening_26813.ris", 26813L)
ghg <- read_ris("GHG_ASReview_matched_4255.ris", 4255L)

out <- "Pool1_SoilBEF_GHG_31068.ris"
if (file.exists(out)) stop("输出文件已存在：", out)

pool1 <- c(soilbef, "", ghg)
writeLines(pool1, out, useBytes = TRUE)

stopifnot(identical(read_ris(out, 31068L), pool1))
message("验证通过：26,813 + 4,255 = 31,068 条。输出文件：", out)

#------------------------------------------------------------------------------31068条
#检索结果









































