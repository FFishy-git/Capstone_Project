# Load the necessary package
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Test Score Input"),
  
  sidebarLayout(
    sidebarPanel(
      # Add a reset button
      actionButton("reset", "Reset Preference"),
      actionButton("reset_personal_info", "Reset Personal Info"),

      # input for scores
      numericInput("sat_reading", "SAT Critical Reading Score:", min = 0, max = 800, value = 800),
      numericInput("sat_math", "SAT Math Score:", min = 0, max = 800, value = 800),
      numericInput("sat_writing", "SAT Writing Score:", min = 0, max = 800, value = 800),
      numericInput("act_composite", "ACT Composite Score:", min = 0, max = 36, value = 36),
      
      # input for personal info
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      selectInput("ethnicity", "Ethnicity:", choices = c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN", "UGDS_NHPI", "UGDS_2MOR", "UGDS_NRA", "UGDS_UNKN")), 
      # add a selectInput for the student's state, choise should be full name without capitalization
      selectInput("state", "State:", choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")),

      # add a multi-selectInput for the urbanization of the school, where there is a choice for selecting all
      selectInput("urbanization", "Urbanization:", choices = c("Select All", "City: Large", "City: Midsize", "City: Small", "Suburb: Large", "Suburb: Midsize", "Suburb: Small", "Town: Fringe", "Town: Distant", "Town: Remote", "Rural: Fringe", "Rural: Distant", "Rural: Remote"), multiple = TRUE),

      # add a bar for the minimal and maximal cost of attendance
      sliderInput("min_cost", "Minimal Cost of Attendance:", min = 0, max = 150000, value = 0, step = 2000),
      sliderInput("max_cost", "Maximal Cost of Attendance:", min = 0, max = 150000, value = 150000, step = 2000),

      # add a bar for the minimal admission rate and maximal admission rate
      sliderInput("min_admission_rate", "Minimal Admission Rate:", min = 0, max = 1, value = 0),
      sliderInput("max_admission_rate", "Maximal Admission Rate:", min = 0, max = 1, value = 1),

      # add a bar for the strength of ethnic preference
      sliderInput("ethnicity_strength", "Strength of Ethnicity Preference:", min = -1.0, max = 1.0, value = 0.0, step = 0.1),
      # add a bar for the strength of earning preference
      sliderInput("financial_aid_strength", "Strength of Financial Aid Preference:", min = -1.0, max = 1.0, value = 0.0, step = 0.1),
      # add a bar for the strength of financial aid preference
      sliderInput("earning_strength", "Strength of Earning Preference:", min = -1.0, max = 1.0, value = 0.0, step = 0.1)
    ),

    mainPanel(
      tableOutput("summary"),
      tableOutput("rank")
    )
  )
)

truncated_dnorm <- function(x, mu, sigma){
  # mu, sigma are vectors of the same length, and x is a number
  
  # the returned value will be:
  # 1. if sigma is Inf, then return 1
  # 2. if sigma is not Inf, and if x < mu, return exp(-(x - mu)^2 / (2 * sigma^2))
  # 3. if sigma is not Inf, and if x >= mu, return 1
  
  # create an empty vector of the same length as x, mu, and sigma
  res <- rep(0, length(mu))
  # if sigma is Inf, then return 1
  res[sigma == Inf] <- 1
  # if sigma is not Inf, and if x < mu, return exp(-(x - mu)^2 / (2 * sigma^2))
  pos <- sigma != Inf & x < mu
  res[pos] <- exp(-((x - mu[pos]) ^ 2 / (2 * sigma[pos] ^ 2)) )
  # if sigma is not Inf, and if x >= mu, return 1
  pos <- sigma != Inf & x >= mu
  res[pos] <- 1

  return(res)
}

predict_score_likelihood <- function(s, data){
  # s is a list with the following format:
  # s = list(
  #   SAT.Critical.Reading = 500,
  #   SAT.Math = 500,
  #   SAT.Writing = 500,
  #   ACT.Composite = 20
  # )

  # Compute the likelihood for each school using the truncated_dnorm function
  item_list <- c("SAT.Critical.Reading", "SAT.Math", "SAT.Writing", "ACT.Composite")
  likelihood <- rep(1, nrow(data))
  for (i in 1:length(item_list)) {
    # get the mean and standard deviation from the data
    mu <- data[[paste(item_list[i], "likelihood.mean", sep = ".")]]
    sd <- data[[paste(item_list[i], "likelihood.sd", sep = ".")]]
    
    # compute the likelihood for each school
    likelihood <- truncated_dnorm(s[[item_list[i]]], mu, sd) * likelihood
  }
  return(likelihood)
}

predict_gender_likelihood <- function(g, data){
  gender_col_name <- "Percent.of.undergraduate.enrollment.that.are.women"
  female_percentage <- data[[gender_col_name]] / 100
  
  # if g == "female" then g_percentage <- female_percentage otherwise g_percentage <- 1 - female_percentage
  # likelihood <- rep(0, nrow(data))
  likelihood <- if (g == "female") {
    female_percentage / pmax(female_percentage, 1-female_percentage)
  } else {
    (1 - female_percentage) / pmax(female_percentage, 1-female_percentage)
  }
  return(likelihood)
}

predict_BF_ethnicity <- function(ethnicity, data){
  # ethnicity_ls <- c(
  #   "UGDS_WHITE",
  #   "UGDS_BLACK",
  #   "UGDS_HISP",
  #   "UGDS_ASIAN",
  #   "UGDS_AIAN",
  #   "UGDS_NHPI",
  #   "UGDS_2MOR",
  #   "UGDS_NRA",
  #   "UGDS_UNKN"
  # )
  prob_H <- data[["School_Prob"]]
  prob_E_given_H <- data[[ethnicity]]
  # calculate Bayesian factor
  BF = log(prob_E_given_H / sum(prob_E_given_H * prob_H))
  return(BF)
}
get_BF <- function(prob_E_given_H, prob_H){
  # calculate Bayesian factor
  BF = log(prob_E_given_H / sum(prob_E_given_H * prob_H))
  return(BF)
}

predict_BF_financial_aid <- function(data){
  BF <- get_BF(data[["Percent.of.freshmen.receiving.any.financial.aid"]], data[["School_Prob"]])
  return(BF)
}

predict_BF_earning <- function(data){
  BF <- get_BF(data[["gt_25k_p6"]], data[["School_Prob"]])
  # if BF is NaN or Inf, then set it to 0
  BF[is.na(BF) | is.infinite(BF)] <- 0
  return(BF)
}

predict_cost <- function(in_state, data){
  tuition_fees <- data[["Tuition.and.fees..2013.14"]]
  cost_in_state <- data[["Total.price.for.in.state.students.living.on.campus.2013.14"]]
  cost_out_state <- data[["Total.price.for.out.of.state.students.living.on.campus.2013.14"]]
  # decide the cost based on the in_state boolean vector for each school
  cost <- cost_out_state
  cost[in_state] <- cost_in_state[in_state]
  # if cost is NaN or Inf, then set it to 0
  cost[is.na(cost) | is.infinite(cost)] <- 0
  cost <- cost + tuition_fees
  # if cost is NaN or Inf, then set it to 0
  cost[is.na(cost) | is.infinite(cost)] <- 0
  return(cost)
}

# Define the server logic
server <- function(input, output, session) {
    onStart <- function() {
    # Load the data
    IPEDS_data <- read.csv("data/cleaned/IPEDS_filled.csv")
    PP_data <- read.csv("data/cleaned/MERGED2013_filtered.csv")
    
    # Merge the datasets on the common column
    merged_data <- merge(IPEDS_data, PP_data, by.x = "ID.number", by.y = "UNITID")
    
    scorecard <- read.csv("data/cleaned/Scorecard_filtered_2011.csv")
    # get column "gt_25k_p6"
    gt_25k_p6 <- scorecard[["gt_25k_p6"]]
    # Add a new column to the merged dataset
    merged_data[, "gt_25k_p6"] <- gt_25k_p6
    # Return the merged dataset
    return(merged_data)
  }
  
  data <- onStart()

  # create a reactive list of scores
  s <- reactive({
    list(
      SAT.Critical.Reading = input$sat_reading,
      SAT.Math = input$sat_math,
      SAT.Writing = input$sat_writing,
      ACT.Composite = input$act_composite, 
      Gender = input$gender,
      Ethnicity = input$ethnicity
    )
  })

    # call the predict_score_likelihood function
    score_likelihood <- reactive({
      predict_score_likelihood(s(), data)
    })
    # predict_gender_likelihood function
    gender_likelihood <- reactive({
      predict_gender_likelihood(input$gender, data)
  })
    # compute the Bayesian Factor for ethnicity
    BF_ethnicity <- reactive({
      predict_BF_ethnicity(input$ethnicity, data)
    })
    # compute the Bayesian Factor for financial aid
    BF_financial_aid <- reactive({
      predict_BF_financial_aid(data)
    })
    # compute the Bayesian Factor for earning
    BF_earning <- reactive({
      predict_BF_earning(data)
    })

    # Decide whether the student is in-state or out-of-state based on the column "st_fips" and predict the cost of attendance
    cost <- reactive({
      predict_cost(which(data[["st_fips"]] == input$state), data)
    })
    # index the schools with cost lower than the maximal cost and higher than the minimal cost
    cost_satisfication <- reactive({
      Cost <- predict_cost(which(data[["st_fips"]] == input$state), data)
      return (Cost <= input$max_cost & Cost >= input$min_cost)
    })
    # index the schools with admission rate lower than the maximal admission rate and higher than the minimal admission rate
  
    admission_rate_satisfication <- reactive({
      Admission_rate <- predict_score_likelihood(s(), data)
      return (Admission_rate <= input$max_admission_rate & Admission_rate >= input$min_admission_rate)
    })

    uburbanization_satisfication <- reactive({
      if ("Select All" %in% input$urbanization) {
        return (rep(TRUE, nrow(data)))
      }
      urbanization <- data[["Degree.of.urbanization..Urban.centric.locale."]]
      return (urbanization %in% input$urbanization)
    })

    # index the data that satisfy both cost and admission rate
    get_satisfication <- reactive({
      cost_satisfication() & admission_rate_satisfication() & uburbanization_satisfication()
    })

    # for each school, compute the combined Bayesian Factor 
    BF_combined <- reactive({
      BF_ethnicity() * input$ethnicity_strength * .2 + BF_financial_aid() * input$financial_aid_strength * .5 + BF_earning() * input$earning_strength
    })

  # rank the schools that satisfy the satisfaction by increasing order of the combined Bayesian Factor
  get_ranked_data <- reactive({
    satisfication <- get_satisfication()
    data[, "BF_score"] <- BF_combined()
    data[, "Admission_Rate"] <- predict_score_likelihood(s(), data)
    data[, "Cost"] <- predict_cost(which(data[["st_fips"]] == input$state), data)
    
    data_satisfication <- data[satisfication, ]
    # rank by increasing order of the combined Bayesian Factor
    data_satisfication[, "Rank"] <- rank(-data_satisfication[["BF_score"]], ties.method="first")
    # sort the data by rank
    data_satisfication <- data_satisfication[order(data_satisfication[["Rank"]]), ]
    return(data_satisfication)
  })

  # output$summary <- renderTable({
  #   data.frame(
  #     ScoreLikelihood = score_likelihood(),
  #     GenderLikelihood = gender_likelihood(),
  #     BFEthnicity = BF_ethnicity(), 
  #     BFFinancialAid = BF_financial_aid(), 
  #     BFEarning = BF_earning(), 
  #     Cost = cost(), 
  #     CostSatisfication = cost_satisfication(),
  #     admission_rate_satisfication = admission_rate_satisfication(), 
  #     satisfaction = satisfaction(), 
  #     BFCombined = BF_combined()
  #   )
  # })
  output$rank <- renderTable({
    data_satisfication <- get_ranked_data()
    data.frame(
      Rank = data_satisfication[["Rank"]],
      University = data_satisfication[["INSTNM"]],
      Cost = data_satisfication[["Cost"]], 
      Admission_Rate = data_satisfication[["Admission_Rate"]],
      BF_Score = data_satisfication[["BF_score"]]
    )
})

  # Reset the slider inputs when the reset button is clicked
  observeEvent(input$reset, {
    updateSliderInput(session, "min_cost", value = 0)
    updateSliderInput(session, "max_cost", value = 150000)
    updateSliderInput(session, "min_admission_rate", value = 0)
    updateSliderInput(session, "max_admission_rate", value = 1)
    updateSliderInput(session, "ethnicity_strength", value = 0.0)
    updateSliderInput(session, "financial_aid_strength", value = 0.0)
    updateSliderInput(session, "earning_strength", value = 0.0)
    # Add other slider inputs that you want to reset here...
  })
  
  # Reset the checkbox inputs when the reset button is clicked, update SAT and ACT scores, 
  # and reset the personal info including state, gender, ethnicity, and urbanization
  observeEvent(input$reset_personal_info, {
    updateCheckboxGroupInput(session, "ethnicity", selected = "UGDS_WHITE")
    updateCheckboxGroupInput(session, "urbanization", selected = NULL)
    updateSelectInput(session, "state", selected = "Alabama")
    updateRadioButtons(session, "gender", selected = "Male")
    updateNumericInput(session, "sat_reading", value = 800)
    updateNumericInput(session, "sat_math", value = 800)
    updateNumericInput(session, "sat_writing", value = 800)
    updateNumericInput(session, "act_composite", value = 800)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)