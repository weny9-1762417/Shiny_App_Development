# UI for APAN5310 Final Project ShinyApp -- team Vancouver

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Chat Box----
  actionButton("show_chat", "Chat with Us",
               style = "position:fixed; right:0%; top:50%; color:red; transform:translateY(-50%); z-index:100;"),
  
  # Chat box container
  div(id = "chat_container",
      style = "display:none; position:fixed; bottom:0; right:0; width:400px; height:500px; background-color:white; border:1px solid #aaa;",
      actionButton("close_chat", "X", 
                   style = "position:absolute; top:5px; right:5px; background-color:#f44336; color:white; border:none;"),
      h3("Chat with Us", style = "text-align: center;"),
      p("Welcome to Vancouver Aviators Website! How can we assist you today?", style = "text-align: center;"),
      
      div(style = "display: flex; flex-direction: column; align-items: center; justify-content: space-around;",
          actionButton("info1", "Purchase Ticket", style = "width: 200px;"),
          actionButton("info2", "Schedule", style = "width: 200px;"),
          actionButton("info3", "Vancouver Tour", style = "width: 200px;"),
          actionButton("support", "Support", style = "width: 200px;")
      ),
      br(), br(),
      div(style = "display: flex; justify-content: center; align-items: center;",
          textAreaInput("chat_input", NULL, placeholder = "Please type your message here...", rows = 4, width = "calc(70% - 10px)"), 
          actionButton("send_msg", "Send", style = "margin-left: 10px; margin-top: -15px;")
      )
  ),
  
  navbarPage(
    # Major layout----
    id = 'tabs',
    windowTitle = 'Vancouver Aviator',
    position = 'fixed-top',
    collapsible = TRUE,
    inverse = FALSE,
    theme = shinytheme('united'),
    uiOutput('uiBkgrd'),
  
    # Home Page----
    tabPanel(
      value = 1,
      title = 'Home',
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      uiOutput('uiHome')
    ),
  
    # Team page----
    navbarMenu(
      title = 'Team Roster',
      tabPanel(
        value = 2.1,
        title = 'Head Coach',
        uiOutput('uiCoach')
      ),
      tabPanel(
        value = 2.2,
        title = 'Players',
        uiOutput('uiPlayers')
      )
    ),
  
    # Rogers Arena(Stadium for our team) page----
    navbarMenu(
      title = 'Rogers Arena',
      tabPanel(
        value = 3.1,
        title = 'Tickets',
        uiOutput('uiTickets')
      ),
      tabPanel(
        value = 3.2,
        title = 'Concerts',
        uiOutput('uiEvents')
      ),
      tabPanel(
        value = 3.3,
        title = 'Games',
        uiOutput('uiGames')
      ),
      tabPanel(
        value = 3.4,
        title = 'About Rogers Arena',
        uiOutput('uiArena')
      )
    ),
    
    # Visit Vancouver Page----
    navbarMenu(
      title = 'Visit Vancouver',
      tabPanel(
        value = 4.1,
        title = 'Museums and Activities',
        uiOutput('uiVAN1')
      ),
      tabPanel(
        value = 4.2,
        title = 'Trip Recommendation',
        uiOutput('uiVAN2')
      ),
      tabPanel(
        value = 4.3,
        title = "Restaurants and Bars",
        uiOutput('uiVAN3')
      ),
    
      tabPanel(
        value = 4.4,
        title = 'Shopping',
        uiOutput('uiVAN4')
      ),
    
      tabPanel(
        value = 4.5,
        title = 'Where to Stay',
        uiOutput('uiVAN5')
      )
    ),

    # Fan Zone Page----
    navbarMenu(
      title = 'Fan Zone',
      tabPanel(
        value = 5.1,
        title = 'Newsletter',
        uiOutput('uiNewsletter')
      ),
      tabPanel(
        value = 5.2,
        title = 'Mobile App',
        uiOutput('uiMobile')
      ),
      tabPanel(
        value = 5.3,
        title = 'Fan Events',
        uiOutput('uiFanEvents')
      ),
      tabPanel(
        value = 5.4,
        title = 'Shop',
        # display products
        uiOutput('uiShop'),
        # Display Cart
        tableOutput("cartTable"),
        textOutput("totalPrice"),
        actionButton("clearCartButton", "Clear Cart"),
        actionButton("purchaseButton", "Purchase")
      ),
      tabPanel(
        value = 5.5,
        title = 'Fan Feedback',
        uiOutput('uiFanFeed')
      )
    ),
  
    # Community Page----
    tabPanel(
      value = 6,
      title = 'Our Community',
      slickROutput("image_carousel"),
      uiOutput('uiCommun')
    ),
  
    # Partner Page----
    tabPanel(
      value = 7,
      title = 'Partnerships',
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      uiOutput('uiPartners')
    ),
    
    # Vancouver & Basketball page----
    navbarMenu(
      #title = 'Vancouver: From Skyline to Baseline',
      title = 'HoopCity',
      tabPanel(
        value = 8.1,
        title = 'Basketball History',
        uiOutput('uiBask')
      ),
      tabPanel(
        value = 8.2,
        title = 'About Vancouver',
        uiOutput('uiAboutVan')
      )
    ),
    
    # About us Page----
      tabPanel(
        value = 9,
        title = 'About Us',
        uiOutput('uiOurTeam')
      )
  )
  
)