pacman::p_load(tidyverse,
               readxl,
               lubridate)

col.label <- 
  c("year",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "type",
    "group")

select.raw <- 14:63

select.column <- c(1,3:14)

raw.vacancy.full <-
  read_excel("data/第6表.xlsx",
             sheet = "第６表ー２（パート除く）") %>%
  .[select.raw,select.column] |> 
  mutate(type = "求人",
         group = "フルタイム")

colnames(raw.vacancy.full) <- col.label

raw.seeker.full <-
  read_excel("data/第7表.xlsx",
             sheet = "第７表ー２（パート除く）") %>%
  .[select.raw,select.column] |> 
  mutate(type = "求職",
         group = "フルタイム")

colnames(raw.seeker.full) <- col.label

raw.hir.full <-
  read_excel("data/第8表.xlsx",
             sheet = "第８表ー２（パート除く）") %>%
  .[select.raw,select.column] |> 
  mutate(type = "新規就職",
         group = "フルタイム")

colnames(raw.hir.full) <- col.label


raw.vacancy.part <-
  read_excel("data/第6表.xlsx",
             sheet = "第６表ー３（パート）") %>%
  .[select.raw,select.column] |> 
  mutate(type = "求人",
         group = "パートタイム")

colnames(raw.vacancy.part) <- col.label

raw.seeker.part <-
  read_excel("data/第7表.xlsx",
             sheet = "第７表ー３（パート）")  %>%
  .[select.raw,select.column] |> 
  mutate(type = "求職",
         group = "パートタイム")

colnames(raw.seeker.part) <- col.label

raw.hir.part <-
  read_excel("data/第8表.xlsx",
             sheet = "第８表ー３（パート）") %>%
  .[select.raw,select.column] |> 
  mutate(type = "新規就職",
         group = "パートタイム")

colnames(raw.hir.part) <- col.label

df <-
  rbind(raw.hir.full,
        raw.hir.part,
        raw.vacancy.full,
        raw.vacancy.part,
        raw.seeker.full,
        raw.seeker.part
  ) |> 
  pivot_longer(cols = 2:13,
               names_to = "month",
               values_to = "n") |> 
  mutate(n = n |> as.numeric(),
         year = year |> str_sub(1,4) |> as.numeric(),
         month = month |> as.numeric(),
         quaterly = month |> cut(c(0,3,6,9,12), labels = c(1,2,3,4)),
         date = yq(str_c(year,quaterly,sep = ":Q"))
  ) |> 
  group_by(date,type,group) |> 
  mutate(n = n |> sum()) |> 
  ungroup() |> 
  distinct(year,quaterly,date,type,n,group) |> 
  spread(key = type, value = n) |> 
  arrange(quaterly,group) |> 
  group_by(quaterly,group) |> 
  mutate(求人 = (求人- lag(求人))/lag(求人),
           求職 = (求職 - lag(求職))/lag(求職),
           新規就職 = (新規就職 - lag(新規就職))/lag(新規就職)
  ) |> 
  ungroup() |> 
  pivot_longer(cols = c(5:7),
               names_to = "type",
               values_to = "N") |> 
  na.omit()

fig <-
  df |> 
  filter(year >= 2000) |> 
  mutate(符号 = if_else(N >= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = N,
             fill = 符号)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  facet_grid(type ~ group) +
  ylab("") +
  xlab("") +
  theme_bw(base_family = "HiraKakuPro-W3") 

fig
