get_target_allocation = function(age = 30, merge_mid_small = FALSE){
  # Each year, Cash Equivalents are increased by 1% and the Non Cash Equivalants 
  # are reduced by 1% using the same ratio as they were in the reference year
  
  reference_age = 30
  current_age = age
  
  if (merge_mid_small == FALSE){
    target_allocation = tribble(~group, ~asset_class, ~reference_alloc, ~target_alloc,
                                "Non Cash Eq", "Large Cap", 40, NA,
                                "Non Cash Eq", "Mid Cap", 10, NA,
                                "Non Cash Eq", "Small Cap", 10, NA,
                                "Non Cash Eq", "Intl - Developed", 20*(2/3), NA,
                                "Non Cash Eq", "Intl - Emerging", 20*(1/3), NA,
                                "Non Cash Eq", "REIT", 5, NA,
                                "Cash Eq", "Bond", reference_age - 15, current_age - 15, # age - 15 where reference age = 30
                                "Cash Eq", "Cash", 0, 0
                                ) 
  }
  if (merge_mid_small == TRUE){
    target_allocation = tribble(~group, ~asset_class, ~reference_alloc, ~target_alloc,
                                "Non Cash Eq", "Large Cap", 40, NA,
                                "Non Cash Eq", "Mid - Small Cap", 20, NA,
                                "Non Cash Eq", "Intl - Developed", 20*(2/3), NA,
                                "Non Cash Eq", "Intl - Emerging", 20*(1/3), NA,
                                "Non Cash Eq", "REIT", 5, NA,
                                "Cash Eq", "Bond", reference_age - 15, current_age - 15, # age - 15 where reference age = 30
                                "Cash Eq", "Cash", 0, 0
                                )
  }
    
  # Non Cash and Equivalents
  reference_non_cash_eq = 100 - (target_allocation %>% filter(group == 'Cash Eq') %>% 
                                   summarise(total = sum(reference_alloc)) %>% pluck(1))
  
  current_non_cash_eq = 100 - (target_allocation %>% filter(group == 'Cash Eq') %>%
                                 summarise(total = sum(target_alloc)) %>% pluck(1))
  
  # Do not round here as it can cause issues in other code that is looking to compute exact values
  target_allocation = target_allocation %>% 
    mutate(target_alloc = if_else(group == "Cash Eq", target_alloc, current_non_cash_eq/reference_non_cash_eq * reference_alloc)#,
           # target_alloc = round(target_alloc,2),
           # reference_alloc = round(reference_alloc,2)
           )  
}