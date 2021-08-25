
# Library ----

pacman::p_load("tidyverse",
               "readxl",
               "lubridate",
               "tidymodels")

# Data ----

number.industry = 68

year <- c(rep(2012,10),
          rep(2013,12),
          rep(2014,12),
          rep(2015,12),
          rep(2016,12),
          rep(2017,12),
          rep(2018,12),
          rep(2019,12),
          rep(2020,12),
          rep(2021,6)
)

month <- c(3:12,
           rep(1:12,8),
           1:6)

sel.row <- 5:72
sel.col <- c(1,19:130)
re.sel.col <- 2:113

raw.vacancy <-
  read_excel("data/第21表.xlsx",
             sheet = "第２１表ー２　有効求人（パート含む常用）") %>%
  .[sel.row,sel.col] |> 
  pivot_longer(cols = re.sel.col,
               values_to = "n") |> 
  mutate(year = rep(year, number.industry),
         month = rep(month,number.industry),
         type = "求人",
         occupation = `職業別有効求人数（パートタイムを含む常用）`
  ) |> 
  select(-`職業別有効求人数（パートタイムを含む常用）`,-name)

raw.seeker <-
  read_excel("data/第21表.xlsx",
             sheet = " 第２１表ー４　有効求職（パート含む常用）") %>%
  .[sel.row,sel.col] |> 
  pivot_longer(cols = re.sel.col,
               values_to = "n") |> 
  mutate(year = rep(year, number.industry),
         month = rep(month,number.industry),
         type = "求職",
         occupation = `職業別有効求職者数（パートタイムを含む常用）`
  ) |> 
  select(-`職業別有効求職者数（パートタイムを含む常用）`,-name)

raw.hir <-
  read_excel("data/第21表.xlsx",
             sheet = "第２１表ー５　 就職件数（パート含む常用）") %>%
  .[sel.row,sel.col] |> 
  pivot_longer(cols = re.sel.col,
               values_to = "n") |> 
  mutate(year = rep(year, number.industry),
         month = rep(month,number.industry),
         type = "新規就職",
         occupation = `職業別就職件数（パートタイムを含む常用）`
  ) |> 
  select(-`職業別就職件数（パートタイムを含む常用）`,-name)

df <-
  rbind(raw.hir,
        raw.vacancy,
        raw.seeker) |> 
  filter(occupation != "職業計" &
           occupation != "管理的職業" &
           occupation != "専門的・技術的職業" &
           occupation != "事務的職業" &
           occupation != "販売の職業" &
           occupation != "サービスの職業" &
           occupation != "輸送・機械運転の職業" &
           occupation != "生産工程の職業" &
           occupation != "建設・採掘の職業" &
           occupation != "運搬・清掃・包装等の職業"&
           occupation != "分類不能の職業") |> 
  mutate(quaterly = month |> cut(c(0,3,6,9,12), labels = c(1,2,3,4)),
         date = yq(str_c(year,quaterly,sep = ":Q")),
         n = as.numeric(n)
  ) |> 
  group_by(date,type,occupation) |> 
  mutate(n = n |> sum()) |> 
  ungroup() |> 
  distinct(year,quaterly,date,type,n,occupation) |> 
  pivot_wider(names_from = type,
              values_from = n) |> 
  group_by(date) |> 
  mutate(総求職 = sum(求職),
            総求人 = sum(求人),
            総新規就職 = sum(新規就職)) |> 
  ungroup() |> 
  filter(year >= 2019)

df.agre <-
  df |> 
  group_by(occupation) |> 
  mutate(share.vacancy = sum(求人),
         share.seeker = sum(求職)) |> 
  ungroup() |> 
  distinct(occupation,
           share.vacancy,
           share.seeker) |> 
  mutate(group = if_else(share.vacancy >= quantile(share.vacancy, prob = 0.9) |
                           share.seeker >= quantile(share.seeker, prob = 0.9),
                         occupation,
                         "その他"
                         )
         )

df |> 
  left_join(df.agre,
            by = "occupation")  |> 
  filter(group != "その他") |> 
  ggplot(aes(x = date,
             y = 求人/総求人,
             color = "求人")
  ) +
  geom_line() + 
  geom_point() +
  geom_line(aes(y =求職/総求職,
                color = "求職")) + 
  geom_point(aes(y =求職/総求職,
                 color = "求職")) +
  geom_vline(xintercept = yq(str_c(2020,2,sep = ":Q")))+
  facet_wrap(~group,
             ncol = 2,
             scales = "free_y") +
  theme_bw(base_family = "HiraKakuPro-W3")


# Compare 2019-2020 ----

df.2019 <-
  rbind(raw.hir,
        raw.vacancy,
        raw.seeker) |> 
  filter(occupation != "職業計" &
           occupation != "管理的職業" &
           occupation != "専門的・技術的職業" &
           occupation != "事務的職業" &
           occupation != "販売の職業" &
           occupation != "サービスの職業" &
           occupation != "輸送・機械運転の職業" &
           occupation != "生産工程の職業" &
           occupation != "建設・採掘の職業" &
           occupation != "運搬・清掃・包装等の職業"&
           occupation != "分類不能の職業") |> 
  mutate(quaterly = month |> cut(c(0,3,6,9,12), labels = c(1,2,3,4)),
         date = yq(str_c(year,quaterly,sep = ":Q")),
         n = as.numeric(n)
  ) |> 
  group_by(date,type,occupation) |> 
  mutate(n = n |> sum()) |> 
  ungroup() |> 
  distinct(year,quaterly,date,type,n,occupation) |> 
  pivot_wider(names_from = type,
              values_from = n) |> 
  group_by(date) |> 
  mutate(総求職 = sum(求職),
            総求人 = sum(求人),
            総新規就職 = sum(新規就職)) |> 
  ungroup() |> 
  filter(date <= yq(str_c(2020,1,sep = ":Q"))) |> 
  mutate(seeker.0 = 求職,
         vacancy.0 = 求人,
         hir.0 = 新規就職) |> 
  select(quaterly,
         seeker.0,
         vacancy.0,
         hir.0,
         occupation)

df.com <-
  df |> 
  left_join(df.2019,
            by = c("quaterly",
                   "occupation"))

df.com.agg <-
  df.com |> 
  filter(date >= yq(str_c(2020,2,sep = ":Q"))) |> 
  group_by(occupation) |> 
  mutate(increase.seeker = sum(求職) - sum(seeker.0)) |>
  ungroup() |> 
  distinct(occupation, increase.seeker) |> 
  mutate(group = if_else(increase.seeker > quantile(increase.seeker, prob = 0.8),
                         occupation,
                         "other")
         )


fig <-
  df |> 
  left_join(df.com.agg,
            by = "occupation")  |> 
  filter(group != "other") |> 
  ggplot(aes(x = date,
             y = 求人/1000,
             color = "求人")
  ) +
  geom_line() + 
  geom_point() +
  geom_line(aes(y =求職/1000,
                color = "求職")) + 
  geom_point(aes(y =求職/1000,
                 color = "求職")) +
  geom_vline(xintercept = yq(str_c(2020,2,sep = ":Q")))+
  facet_wrap(~group,
             ncol = 2,
             scales = "free_y") +
  ylab("千件") +
  xlab("") +
  theme_bw(base_family = "HiraKakuPro-W3")

ggsave("")