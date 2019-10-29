library(readr)
library(here)
# Main data sets

summary_all <- read_csv(here("www", "data", "data-for-app.csv"), col_types = cols(paper_id_list1 = col_character(), paper_id_list2 = col_character(), paper_id_list3 = col_character()))
references <- read_csv(here("www", "data", "references-for-app.csv"), col_types = cols(Paper_id = col_integer()))

summary_all <- arrange(summary_all, group_metric)
summary <- select(summary_all[1:20,], Review, Group_RV, group_metric, paper_id_list1 )
summary <- summary %>% rename(Group_RV = Groups)

names(summary)[2] <- "Group"
names(summary)[3] <- "Outcome"
names(summary)[4] <- "Paper_id_list"

write.csv(summary, file = "/Users/LWA/Desktop/github/midwesternag_synthesis/www/data/test_summary.csv", row.names = FALSE)



test <- separate_rows(summary_all, paper_id_list1, convert = TRUE)

test2 <- test %>%
          group_by(main_group, paper_id_list1)%>%
          summarise(group_metric1 = paste(unique(group_metric), collapse = ","),
                    Review1 = paste(unique(Review), collapse = ", "))

  df <- data.frame(
    x = 1:3,
    y = c("a", "d,e,f", "g,h"),
    z = c("1", "2,3,4", "5,6"),
    stringsAsFactors = FALSE
  )
  separate_rows(df, y, z, convert = TRUE)
  