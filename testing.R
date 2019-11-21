library(tidyverse)

out_of=50
"
1a
28.50
21.75
24.00
29.75
24.25
38.50
37.50
28.00
31.75
38.00
35.75
35.50
40.50
41.25
44.50
39.50
40.75
40.50
40.00
45.25
44.75
42.00
44.75
0.00 NS W" %>% 
{ . } -> raw_values


strsplit("[[:space:]]") %>%
pluck(1) %>%
enframe( name=NULL, value='raw_input') %>%
filter( !str_detect( raw_input, '^[[:space:]]*$')) %>%
mutate( raw_input = str_replace_all(raw_input, ' *', '')) %>%
mutate( special = ifelse( raw_input %in% c('NS'), 'NS', 
                         ifelse( raw_input %in% c('W'), 'W', 
                                ifelse( str_detect( raw_input, '^[0-9.]*$' ), 'V', 'ERRORS' )))) %>% 
{ . } -> df

n_input = nrow(df)

calc_grade = function( mark ) {
  grades=c('D','C','B')
  if (mark < 50 ) return('N')
  if (mark >= 80 ) return('A')
  grades[ floor(mark/10)-4 ]
}

em_percent = function( n ) {
  round( n*100, 2) %>%
    as.character() %>%
    paste('%', sep='')
}


df %>% 
  filter(special == 'V') %>%
  mutate( mark = as.numeric(raw_input) / out_of*100) %>%
  rowwise() %>%
  mutate( grade = calc_grade(mark)) %>%
  ungroup() %>%
  count(grade) %>%
  mutate( percent = em_percent( n / n_input)  ) %>% 
  { . } -> df1

df %>% 
  filter(special != 'V') %>%
  count(special) %>%
  rename( grade=special ) %>%
  mutate( percent = em_percent( n / n_input)  ) %>% 
  { . } -> df2

bind_rows( df1, df2) %>%
  mutate( n=as.character(n)) %>%
  pivot_wider( names_from=grade, values_from=n, -percent ) %>% 
  { . } -> df1_w


bind_rows( df1, df2) %>%
  pivot_wider( names_from=grade, values_from=percent, -n ) %>% 
  { . } -> df2_w

bind_rows( df1_w, df2_w)

