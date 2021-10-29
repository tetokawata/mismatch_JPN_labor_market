
# Library ----

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               Rfast)

# Data ----

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

# Aggregate level ----

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

unique(df$type)

fig <-
  df |> 
  filter(year >= 2000) |> 
  mutate(増減 = if_else(N >= 0, "増加", "減少")) |> 
  mutate(type = factor(type, 
                        levels = c("求人","求職","新規就職"))
         ) |> 
  ggplot(aes(x = date,
             y = N,
             fill = 増減)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  scale_x_date(breaks = c(make_date(2000,1),
                          make_date(2010,1),
                          make_date(2020,1)
                          ),
               labels = c("2000",
                          "2010",
                          "2020")
               ) +
  facet_grid(type ~ group) +
  ylab("") +
  xlab("") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "none")

fig

ggsave("figure/aggregate_index.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)


# Aggregate rate ----

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
  mutate(求人倍率 = (求人/求職- lag(求人/求職))/lag(求人/求職),
           入職率 = (新規就職/求職 - lag(新規就職/求職))/lag(新規就職/求職)
  ) |> 
  ungroup() |> 
  pivot_longer(cols = c(8:9),
               names_to = "type",
               values_to = "N") |> 
  na.omit()

fig <-
  df |> 
  filter(year >= 2000) |> 
  mutate(増減 = if_else(N >= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = N,
             fill = 増減)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  scale_x_date(breaks = c(make_date(2000,1),
                          make_date(2010,1),
                          make_date(2020,1)
  ),
  labels = c("2000",
             "2010",
             "2020")
  ) +
  facet_grid(type ~ group) +
  ylab("") +
  xlab("") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "none")

fig

ggsave("figure/aggregate_rate.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)

# Surplus rate ----

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
  group_by(date,type) |> 
  mutate(n = n |> sum()) |> 
  ungroup() |> 
  distinct(year,quaterly,date,type,n) |> 
  spread(key = type, value = n) |> 
  arrange(quaterly) |> 
  group_by(quaterly) |> 
  mutate(サーチ活動からの余剰 = (lag(求職)/lag(求人))*((求人/求職) - (lag(求人)/lag(求職))),
                   入職率の貢献 = (lag(求職)/lag(求人))*(lag(求人)/lag(新規就職))*((新規就職/求職) - (lag(新規就職)/lag(求職))),
                   入職からの余剰の貢献 = (lag(求職)/lag(求人))*(新規就職/求職)*((求人/新規就職) - (lag(求人)/lag(新規就職)))
  ) |> 
  ungroup() |> 
  pivot_longer(cols = c(7:9),
               names_to = "type",
               values_to = "N") |> 
  na.omit()

fig <-
  df |> 
  filter(year >= 2000) |> 
  mutate(増減 = if_else(N >= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = N,
             fill = 増減)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  scale_x_date(breaks = c(make_date(2000,1),
                          make_date(2010,1),
                          make_date(2020,1)
  ),
  labels = c("2000",
             "2010",
             "2020")
  ) +
  facet_wrap(~type,
             ncol = 1) +
  ylab("") +
  xlab("") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "none")

fig

ggsave("figure/surplus.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)

# Occupation level ----

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
         date = str_c(year,quaterly,sep = ":Q") |> yq(),
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
  ungroup()

df.agre <-
  df |> 
  group_by(occupation) |> 
  mutate(share = sum(求人)) |> 
  ungroup() |> 
  distinct(occupation,
           share) |> 
  mutate(group = if_else(share <= nth(share, 49),"その他",occupation))


df <-
  df |> 
  left_join(df.agre,
            by = "occupation")

df.select <-
  df |> 
  filter(date >= make_date(2012,4)) |> 
  filter(group != "その他") |> 
  mutate(余剰の割合 = 求人/総求人) 

df.other <-
  df |> 
  filter(date >= make_date(2012,4)) |> 
  filter(group == "その他") |> 
  mutate(余剰の割合 = 求人/総求人) 

fig <-
  ggplot() +
  geom_line(aes(y = 余剰の割合, 
                x = date, 
                group = occupation), 
            data = df.other, 
            colour = alpha("grey", 0.7)
  ) +
  geom_line(aes(y = 余剰の割合, 
                x = date, 
                color = occupation), 
            data = df.select
  ) +
  geom_point(aes(y = 余剰の割合, 
                 x = date, 
                 color = occupation), 
             data = df.select) +
  xlab("") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_legend(ncol = 3))

fig

ggsave("figure/occupation_share.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)


## Total----

df.1 <-
  df |> 
  filter(occupation != "分類不能の職業") |> 
  filter(group != "その他") |> 
  group_by(group, quaterly) |> 
  mutate(change = (lag(総求職)/lag(総求人))*((求人/総求職) - (lag(求人)/lag(総求職)))
  ) |> 
  ungroup() |> 
  filter(date >= make_date(2013,4))

fig <-
  df.1 |> 
  mutate(符号 = if_else(change>= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = change,
             fill = 符号)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  facet_wrap(~group,
             ncol = 2) +
  theme_bw(base_family = "HiraKakuPro-W3") +
  ylim(-0.04,0.03) +
  ylab("") +
  xlab("")

fig


ggsave("figure/occupation_total.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)

## Matching surplus ----

df.1 <-
  df |> 
  filter(occupation != "分類不能の職業") |> 
  filter(group != "その他") |> 
  group_by(group, quaterly) |> 
  mutate(change = (lag(総求職)/lag(総求人))*(新規就職/総求職)*((求人/新規就職) - (lag(求人)/lag(新規就職)))
  ) |> 
  ungroup() |> 
  filter(date >= make_date(2013,4))

fig <-
  df.1 |> 
  mutate(符号 = if_else(change>= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = change,
             fill = 符号)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  facet_wrap(~group,
             ncol = 2) +
  theme_bw(base_family = "HiraKakuPro-W3") +
  ylim(-0.04,0.03)+
  ylab("") +
  xlab("")

fig


ggsave("figure/occupation_surplus.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)

## Finging ----

df.1 <-
  df |> 
  filter(occupation != "分類不能の職業") |> 
  filter(group != "その他") |> 
  group_by(group, quaterly) |> 
  mutate(change = (lag(総求職)/lag(総求人))*(lag(求人)/lag(新規就職))*((新規就職/総求職) - (lag(新規就職)/lag(総求職)))
  ) |> 
  ungroup() |> 
  filter(date >= make_date(2013,4))

fig <-
  df.1 |> 
  mutate(符号 = if_else(change>= 0, "増加", "減少")) |> 
  ggplot(aes(x = date,
             y = change,
             fill = 符号)
  ) +
  geom_bar(stat = "identity") +
  geom_line(aes(y=0)) +
  facet_wrap(~group,
             ncol = 2) +
  theme_bw(base_family = "HiraKakuPro-W3") +
  ylim(-0.04,0.03) +
  ylab("") +
  xlab("")


fig


ggsave("figure/occupation_prob.png",
       fig,
       dpi = "retina",
       width = 6.4, 
       height = 4.8)

