library(shiny)
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
0.00
library(tidyverse)

default = tribble(
                  ~grade, ~n,
                  "A",   0,
                  "B",   0,
                  "C",   0,
                  "D",   0,
                  "N",   0,
                  "NS",   0,
                  "W",   0
)

test = function () {

process_marks('', 100)
process_marks('\n', 100)
process_marks('1\n', 100)

process_marks('1\\', 100)

process_marks('a b 100 70 60 50 1 NS W E', 100)

process_marks('100 70 60 50 1', 100)

raw2df('100 70 60 50 1 NS W E')
grades_have_error('100 70 60 50 1 NS W E')

grades_have_error('100 70 60 50 1 NS W E')
grades_have_error('')
grades_have_error('1\n1')
grades_have_error('1\\n1')
grades_have_error('100 70 60 50 1 NS W E')
grades_have_error('NS W E')

raw2df('100 70 60 50 1 NS W E') %>% filter( special=='V') %>% nrow()
}

#undebug(process_marks)
#undebug(grades_have_error)

################################################################################
#
################################################################################
raw2df = function ( raw_values ) {
  raw_values %>%
    strsplit("[[:space:]]") %>%
    pluck(1) %>%
    enframe( name=NULL, value='raw_input') %>%
    filter( !str_detect( raw_input, '^[[:space:]]*$')) %>%
    mutate( raw_input = str_replace_all(raw_input, ' *', '')) %>%
    mutate( special = ifelse( raw_input %in% c('NS'), 'NS', 
                            ifelse( raw_input %in% c('W'), 'W', 
                                    ifelse( str_detect( raw_input, '^[0-9.]*$' ), 'V', 'ERRORS' ))))
}

################################################################################
#
################################################################################
calc_grade = function( mark ) {
  grades=c('D','C','B')
  if (mark < 50 ) return('N')
  if (mark >= 80 ) return('A')
  grades[ floor(mark/10)-4 ]
}


################################################################################
#
################################################################################
em_percent = function( n ) {
  round( n*100, 2) %>%
    as.character() %>%
    paste('%', sep='')
}



################################################################################
#
################################################################################
process_marks = function( raw_values, out_of ) {

  df = raw2df( raw_values )
  n_input = nrow(df)
  
  df %>% 
    filter(special == 'V') %>%
    mutate( mark = as.numeric(raw_input) / out_of*100) %>%
    rowwise() %>%
    mutate( grade = calc_grade(mark)) %>%
    ungroup() %>%
    count(grade) %>%
    { . } -> df1

  df %>% 
    filter(special != 'V') %>%
    count(special) %>%
    rename( grade=special ) %>%
    { . } -> df2

  bind_rows( df1, df2) %>%
    bind_rows( default) %>%
    group_by( grade ) %>%
    summarise( n = sum(n)) %>%
    mutate( percent = em_percent( n / n_input)) %>%
    mutate( n=as.character(n)) %>% 
    { . } -> df_final

  df_final %>%
    select( grade, percent ) %>%
    mutate( grade= paste(grade, '%')) %>%
    rename( n=percent) %>%
    bind_rows( select( df_final, grade, n )) %>%
    arrange( grade ) %>%
    pivot_wider( names_from=grade, values_from = n) 


}


################################################################################
#
################################################################################
grades_has_something = function( raw_values ) {

  if (str_detect( raw_values, '^[[:space:]]*$'))  return (FALSE)

  TRUE
}

################################################################################
#
################################################################################
grades_has_numbers = function( raw_values ) {

  if( raw2df( raw_values ) %>% filter( special=='V') %>% nrow() ==0) return(FALSE)

  TRUE
}

################################################################################
#
################################################################################
grades_no_errors = function( raw_values ) {
  
  if( raw2df( raw_values ) %>% filter( special=='ERRORS') %>% nrow() !=0) return(FALSE)

  TRUE
}


################################################################################
#
################################################################################

################################################################################
#
################################################################################
function(input, output) {
  shiny::reactive({
    shiny::validate( 
                    shiny::need(grades_has_something( input$raw_values), 
                                "Please paste or enter in grades for summarisation")
    )
    shiny::validate( 
                    shiny::need(grades_has_numbers( input$raw_values), 
                                "Please enter in some grades")
    )
    shiny::validate( 
                    shiny::need(grades_no_errors( input$raw_values), 
                                "Please only enter numbers, NS or W values")
    )
  })

    output$rv <- renderTable({ 
      if(!grades_has_something( input$raw_values))  return(NULL)
      if(!grades_has_numbers( input$raw_values))  return(NULL)
      if(!grades_no_errors( input$raw_values))  return(NULL)
#      browser()
      process_marks( input$raw_values, as.numeric(input$out_of))  
    })

}
