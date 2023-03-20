cdf2_selections <- cdf %>%
  filter(
    common_name == "sugar maple",
    phenophase_description == "Falling leaves",
    year == "2011",
    day_of_year == day_of_year
  )


hist(
  cdf2_selections$day_of_year,
  xlab = 'Day of Year',
  main = 'Distribution of day of year by phenophase',
  breaks = 365,
  xlim=c(0,365),
  col = 'blue',
  border = 'blue'
)

#########

falling_leaves_selection <- falling_leaves %>%
  filter(
    common_name == "sugar maple",
    #phenophase_description == "Falling leaves",
    year == "2011"#,
    #day_of_year == day_of_year
  )


hist(
  falling_leaves_selection$day_of_year,
  xlab = 'Day of Year',
  main = 'Distribution of day of year by phenophase',
  breaks = 365,
  xlim=c(0,365),
  col = 'blue',
  border = 'blue'
)
