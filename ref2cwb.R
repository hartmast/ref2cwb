library(tidyverse)

# list files
f_mlu <- list.files("ref-mlu/", full.names = T)
f_rub <- list.files("ref-rub/", full.names = T)

f <- c(f_mlu, f_rub)

for(j in 1:length(f)) {
  # read data
  d <- readLines(f[j])
  
  # get metadata
  header_name <- gsub("\".*", "", gsub(".*name=\"", "", grep("<cora-header .* name", d, value = T)))
  
  # get header
  header_start <- grep("<header", d)
  header_end   <- grep("</header", d)
  hdr <- d[header_start:header_end]
  
  # remove all quotation marks to avoid syntax problems
  hdr <- gsub("\"", "", hdr)
  
  # get header infos
  hdr <- sub(":", "=\"", hdr)
  hdr <- gsub("(?<=$)", "\"", hdr, perl = T)
  text_header <- paste0("<text id=\"", header_name, "\"", gsub("text=\"", "text_name=\"", gsub("</?header>", "", paste0(hdr, collapse = " "))), ">", collapse = " ")
  
  # get tokens
  token_start <- grep("<tok_anno", d)
  token_end   <- grep("</tok_anno", d)
  
  
  # empty table for dipl, lemma, POS, morph
  cur_tbl <- tibble(
    word      = character(length(token_start)),
    token_id  = character(length(token_start)),
    tok_anno      = character(length(token_start)),
    lemma     = character(length(token_start)),
    pos       = character(length(token_start)),
    pos_lemma = character(length(token_start)),
    morph     = character(length(token_start)),
    lemma_id     = character(length(token_start)),
    anno_type =  character(length(token_start)),
    punc     = character(length(token_start))
  )
  
  
  # fill table
  for(i in 1:length(token_start)) {
    cur <-d[token_start[i]:token_end[i]]
    
    if(length(grep(".*<tok_anno id=\"", cur)) > 0) {
      cur_tbl$token_id[i] <- gsub("\".*", "", gsub(".*<tok_anno id=\"", "", cur[grep("<tok_anno", cur)]))
    } else {
      cur_tbl$token_id[i] <- "-"
    }
    
    if(length(grep(".*<tok_anno.*utf", cur)) > 0) {
      cur_tbl$tok_anno[i] <- gsub("\".*", "", gsub(".*utf=\"", "", cur[grep("<tok_anno", cur)]))
    } else {
      cur_tbl$tok_anno[i] <- "-"
    }
    
    if(length(grep(".*<tok_anno.*ascii", cur)) > 0) {
      
      cur_tbl$word[i] <- gsub("\".*", "", gsub(".*ascii=\"", "", cur[grep("<tok_anno", cur)]))
    } else {
      cur_tbl$word[i] <- "-"
    }
    
    if(length(grep(".*<lemma tag=\"", cur)) > 0) {
      cur_tbl$lemma[i] <- gsub("\".*", "", gsub(".*<lemma tag=\"", "", cur[grep("lemma tag", cur)]))
    } else {
      cur_tbl$lemma[i] <- "-"
    }
    
    if(length(grep(".*<posLemma tag=\"", cur)) > 0) {
      cur_tbl$pos_lemma[i] <- gsub("\".*", "", gsub(".*<posLemma tag=\"", "", cur[grep("posLemma tag", cur)]))
    } else {
      cur_tbl$pos_lemma[i] <- "-"
    }
    
    if(length(grep(".*<morph tag=\"", cur)) > 0) {
      cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<morph tag=\"", "", cur[grep("morph tag", cur)]))
    } else {
      cur_tbl$morph[i] <- "-"
    }
    
    if(length(grep(".*<pos tag=\"", cur)) > 0) {
      cur_tbl$pos[i] <- gsub("\".*", "", gsub(".*<pos tag=\"", "", cur[grep("pos tag", cur)]))
    } else {
      cur_tbl$pos[i] <- "-"
    }
    
    if(length(grep(".*<punc tag=\"", cur)) > 0) {
      cur_tbl$punc[i] <- gsub("\".*", "", gsub(".*<punc tag=\"", "", cur[grep("punc tag", cur)]))
    } else {
      cur_tbl$punc[i] <- "-"
    }
    
    if(length(grep(".*<annoType tag=\"", cur)) > 0) {
      cur_tbl$anno_type[i] <- gsub("\".*", "", gsub(".*<annoType tag=\"", "", cur[grep("annoType tag", cur)]))
    } else {
      cur_tbl$anno_type[i] <- "-"
    }
    #print(i)
    
    
    
  }
  
  
  # create CWB file
  
  # file header
  
  if(j == 1) {
    write.table(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<corpus>"),
                "ref.vrt", row.names = F, col.names = F, quote = F)
  }
  
  
  write.table(text_header,
              "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
  write.table(cur_tbl,
              "ref.vrt", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
  write.table("</text>",
              "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
  
  if(j == length(f)) {
    write.table("</corpus>",
                "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
  }
  
  print(j)
}

# # read data
# d <- readLines(f[1])
# 
# # get metadata
# header_name <- gsub("\".*", "", gsub(".*name=\"", "", grep("<cora-header .* name", d, value = T)))
# 
# # get header
# header_start <- grep("<header", d)
# header_end   <- grep("</header", d)
# hdr <- d[header_start:header_end]
# 
# # get header infos
# hdr <- sub(":", "=\"", hdr)
# hdr <- gsub("(?<=$)", "\"", hdr, perl = T)
# text_header <- paste0("<text id=\"", header_name, "\"", gsub("text=\"", "text_name=\"", gsub("</?header>", "", paste0(hdr, collapse = " "))), ">", collapse = " ")
# 
# # get tokens
# token_start <- grep("<tok_anno", d)
# token_end   <- grep("</tok_anno", d)
# 
# 
# # empty table for dipl, lemma, POS, morph
# cur_tbl <- tibble(
#   word      = character(length(token_start)),
#   token_id  = character(length(token_start)),
#   dipl      = character(length(token_start)),
#   lemma     = character(length(token_start)),
#   pos       = character(length(token_start)),
#   pos_lemma = character(length(token_start)),
#   morph     = character(length(token_start))
# )
# 
# 
# # fill table
# for(i in 1:length(token_start)) {
#   cur <-d[token_start[i]:token_end[i]]
#   
#   if(length(grep(".*<tok_anno id=\"", cur)) > 0) {
#     cur_tbl$token_id[i] <- gsub("\".*", "", gsub(".*<tok_anno id=\"", "", cur[grep("<tok_anno", cur)]))
#   } else {
#     cur_tbl$token_id[i] <- "-"
#   }
#   
#   if(length(grep(".*<tok_anno.*utf", cur)) > 0) {
#     cur_tbl$dipl[i] <- gsub("\".*", "", gsub(".*utf=\"", "", cur[grep("<tok_anno", cur)]))
#   } else {
#     cur_tbl$dipl[i] <- "-"
#   }
#   
#   if(length(grep(".*<tok_anno.*ascii", cur)) > 0) {
#     
#     cur_tbl$word[i] <- gsub("\".*", "", gsub(".*ascii=\"", "", cur[grep("<tok_anno", cur)]))
#   } else {
#     cur_tbl$word[i] <- "-"
#   }
#   
#   if(length(grep(".*<lemma tag=\"", cur)) > 0) {
#     cur_tbl$lemma[i] <- gsub("\".*", "", gsub(".*<lemma tag=\"", "", cur[grep("lemma tag", cur)]))
#   } else {
#     cur_tbl$lemma[i] <- "-"
#   }
#   
#   if(length(grep(".*<posLemma tag=\"", cur)) > 0) {
#     cur_tbl$pos_lemma[i] <- gsub("\".*", "", gsub(".*<posLemma tag=\"", "", cur[grep("posLemma tag", cur)]))
#   } else {
#     cur_tbl$pos_lemma[i] <- "-"
#   }
#   
#   if(length(grep(".*<morph tag=\"", cur)) > 0) {
#     cur_tbl$morph[i] <- gsub("\".*", "", gsub(".*<morph tag=\"", "", cur[grep("morph tag", cur)]))
#   } else {
#     cur_tbl$morph[i] <- "-"
#   }
#   
#   if(length(grep(".*<pos tag=\"", cur)) > 0) {
#     cur_tbl$pos[i] <- gsub("\".*", "", gsub(".*<pos tag=\"", "", cur[grep("pos tag", cur)]))
#   } else {
#     cur_tbl$pos[i] <- "-"
#   }
#   
#   print(i)
#   
#   
#   
# }
# 
# 
# # create CWB file
# 
# # file header
# 
# if(j == 1) {
#   write.table(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<corpus>"),
#               "ref.vrt", row.names = F, col.names = F, quote = F)
# }
# 
# 
# write.table(text_header,
#             "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
# write.table(cur_tbl,
#             "ref.vrt", sep = "\t", row.names = F, col.names = F, quote = F, append = T)
# write.table("</text>",
#             "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
# 
# if(j == length(f)) {
#   write.table("</corpus>",
#               "ref.vrt", row.names = F, col.names = F, quote = F, append = T)
# }
