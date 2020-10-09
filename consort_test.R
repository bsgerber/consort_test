library(DiagrammeR)

# Data

status_table <- data.frame(
  randomized = c(rep("Yes", 602),
                 rep("No", 197)),
  excluded_reason = c(rep(NA, 602), #must match randomized = Yes
                      rep("Did not meet inclusion criteria", 169),
                      rep("Met Exclusion Criteria", 11),
                      rep("Did not Undergo ERCP", 17)),
  arm = c(rep("Placebo", 307),
          rep("Indomethacin", 295),
          rep(NA, 197) # must match randomized = No
  ),
  received_treatment = c(rep("Received treatment as randomized", 307),
                         rep("Received treatment as randomized", 295),
                         rep(NA, 197)),
  completed = c(rep("Yes", 307),
                rep(NA, 1),
                rep("Yes", 294),
                rep(NA, 197)),
  discont_reason = c(rep(NA, 307),
                     rep("Could not hold Suppository", 1),
                     rep(NA, 294),
                     rep(NA, 197)),
  analyzed = c(rep("Yes", 602),
               rep(NA, 197)),
  not_an_reason = rep(NA, 799)
)

# Make consort diagram from data frame

make_consort <- function(df) {
  grViz(
    paste0(
      "digraph consort_diagram { 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        assessed [label = <", nrow(df), " Individuals assessed for eligibility >]
        blank[label = '', width = 0.01, height = 0.01]
        assessed -> blank[ dir = none ];
        
        randomized [label = <", nrow(df[df$randomized == "Yes", ]), " Randomized >]
        excluded[label = <", create_text_for_box(df$excluded_reason, 'Excluded', breakdown1 = T), ">]
        
        blank -> excluded[ minlen = 2 ];
        blank -> randomized;
        { rank = same; blank excluded }",
        create_arms(df),
      "}
    ")
  )
}

# Create diagram arms with boxes

create_arms <- function(df) {
  # Only include if randomized to arms
  arm_data <- df[!is.na(df$arm), ]
  
  # List of arm strings
  arm_strings <- list()
  
  # For each arm create box text
  for (arm in unique(arm_data$arm)) {
    arm_name <- gsub(" ", "_", arm)
    completed_arm_name <- paste0("completed_", arm_name)
    analyzed_arm_name <- paste0("analyzed_", arm_name)
    arm_strings[[arm]] <- paste0(arm_name, "[label = <", 
                create_text_for_box(arm_data[arm_data$arm == arm, "received_treatment"], 
                                    paste('Randomized to receive', arm),
                                    breakdown1 = T),
                ">, group = ", arm_name, "]",
                "randomized -> ", arm_name, ";",
                completed_arm_name, "[label = <",
                create_text_for_box(arm_data[arm_data$arm == arm, "completed"], 
                                    "Completed trial", breakdown1 = F,
                                    arm_data[arm_data$arm == arm, "discont_reason"],
                                    "Did not complete trial", breakdown2 = T),
                ">, group = ", arm_name, "]",
                paste(arm_name, "->", completed_arm_name, ";"),
                analyzed_arm_name, "[label = <",
                create_text_for_box(arm_data[arm_data$arm == arm, "analyzed"], 
                                    "Included in the primary analysis", breakdown1 = F,
                                    arm_data[arm_data$arm == arm, "not_an_reason"],
                                    "Excluded from analysis", breakdown2 = T),
                ">, group = ", arm_name, "]",
                paste(completed_arm_name, "->", analyzed_arm_name, ";")
    )
  }
  return(paste(unlist(arm_strings), collapse = ''))
}

# Create table text for an individual box

create_text_for_box <- function(vec1, box_label1, breakdown1, vec2 = NA, box_label2 = NA, breakdown2 = F) {
  items1 <- na.omit(vec1)
  items2 <- na.omit(vec2)
  t1 <- table(items1)
  t2 <- table(items2)
  s <- paste0('<table border="0" cellborder="0" cellspacing="0">', 
              create_box_header(items1, box_label1),
              if(breakdown1) create_box_rows(t1))
  if (length(items2) > 0) {
    s <- paste0(s, 
                create_box_header(items2, box_label2),
                if(breakdown2) create_box_rows(t2))
  }
  s <- paste0(s, '</table>')
  return(s)
}

# Create table header text for a box

create_box_header <- function(items, label) {
  paste0('<tr><td align = "right">', length(items), '</td>',
         '<td align = "left" colspan = "2">', label, '</td></tr>')
}

# Create table row text for a box

create_box_rows <- function(t) {
  s <- ''
  for (nm in names(t)) {
    s <- paste0(s,
                '<tr>',
                '<td></td>',
                '<td align = "right">', t[nm], '</td>',
                '<td align = "left">', nm, '</td>',
                '</tr>')
  }
  return(s)
}

# Make the consort diagram (main function to call)

make_consort(status_table)
