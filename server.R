#Server for APAN5310 Team Vancouver

server <- function(input, output, session) {
  
  # >>>>>>>>>>>>>>>>>>----
  # reactive values----
  g <- reactiveValues(
    i = 1, # game selected
  )
  
  # keep track of the current museum index
  currentMuseum <- reactiveVal(1)
  
  a <- reactiveValues(
    ev = 1, # event # for events
    r = 0, # the restaurant selected (1 to 10)
  )
  s <- reactiveValues(
    ev = 1) #shopping mall
  
  h <- reactiveValues(
    i = 1, # hotel selected (1 to 5)
  )
  
  m <- reactiveValues(
    i = 1, # member selected (1 to 8)
  )
  
  f <- reactiveValues(
    ev = 1, # event # for events
    r = 0,
  )
  
  p <- reactiveVal(1) # partner selected (1 to 9)
  
  c <- reactiveValues(
    i = 1, # concerts selected 1-4
    ev = 1
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # ui background----
  output$uiBkgrd <- renderUI(
    if (input$tabs == 1) {
      setBackgroundImage('van_home.jpg')
    } else if (input$tabs < 3) {
      setBackgroundImage('team_bg.jpg')
    } else if (input$tabs < 4) {
      setBackgroundImage('stadium/view1.jpeg')
    } else if (input$tabs == 4.1) {
      setBackgroundImage('art.gif')
    } else if (input$tabs == 4.2) {
      setBackgroundImage('trip_bg.jpg')  
    } else if (input$tabs == 4.3) {
      setBackgroundImage('dining.jpg')
    } else if (input$tabs == 4.4) {
      setBackgroundImage('mall_bg.jpg')
    } else if (input$tabs == 4.5) {
      setBackgroundImage('hotel.jpg')
    } else if (input$tabs == 5.1) {
      setBackgroundImage('fan1.jpg')
    } else if (input$tabs == 5.2) {
      setBackgroundImage('fan2.jpg')
    } else if (input$tabs == 5.3) {
      setBackgroundImage('fan2.jpg')
    } else if (input$tabs == 5.4) {
      setBackgroundImage('nba2.gif')
    } else if (input$tabs == 5.5) {
      setBackgroundImage('fan.jpg')  
    } else if (input$tabs == 6) {
      setBackgroundImage('com2.jpg')
    } else if (input$tabs == 7) {
      setBackgroundImage('partners/part.gif')
    } else if (input$tabs == 8.1) {
      setBackgroundImage('bask.webp')
    } else if (input$tabs == 8.2) {
      setBackgroundImage('dem.jpeg')
    } else {
      setBackgroundImage('van2.gif')
    }
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # chat box----
  observeEvent(input$show_chat, {
    shinyjs::hide("show_chat")
    shinyjs::show("chat_container")
  })
  
  observeEvent(input$send_msg, {
    new_msg <- paste("You:", input$chat_input)
    print(new_msg)
    updateTextInput(session, "chat_input", value = "")
  })
  
  observeEvent(input$info1, {
    updateTextInput(session, "chat_input", value = "Please look at the Rogers Arena -> Ticket Page")
  })
  
  observeEvent(input$info2, {
    updateTextInput(session, "chat_input", value = "Our NBA Schedule is currently unavaliable, please refresh later")
  })
  
  observeEvent(input$info3, {
    updateTextInput(session, "chat_input", value = "We recommend three trip routes on our website(Visit Vancouver-> Trip Recommendation)")
  })
  
  observeEvent(input$support, {
    updateTextInput(session, "chat_input", value = "Please contact us via phone: (+1)888,8888886, or via email: vanaviators@gmail.com")
  })
  
  observeEvent(input$close_chat, {
    shinyjs::hide("chat_container")
    shinyjs::show("show_chat")
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiHome page(1)----
  output$uiHome <- renderUI({
    tagList(
      div(h1(class = "shine-effect", id = "home_title", "The Vancouver Aviators"),
          div(id ="logo_container",
              tags$img(id= "logo", src="nba_logo.png", alt="NBA")
          ),
          div(id = "logo_container2",
              tags$img(id = "team_logo", src="vancouver.png", alt="Logo")
          )
      ),
      div(id = 'home_gif')
    )
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiCoach(2.1)----
  output$uiCoach <- renderUI({
      div(
        style = 'padding:30vh 40% 0 40%;',
        align = 'center',
        h2(
          style = 'color:white;',
          'Head Coach'
        ),
        img(
          src = 'head_coach.webp',
          height = '200px',
          width = '200px',
          style = paste0(
            'object-fit:cover; margin:0 10px 0 10px; ',
            'border:solid white 2px; border-radius:50%;',
            'background-color:lightblue;'
          )
        ),
        br(), br(),
        actionBttn(
          inputId = 'coach',
          label = 'Gregg Popovich',
          style = 'unite',
          color = 'primary',
          size = 'md',
          block = FALSE
        )
      )
    }
  )
  
  #_event coach button----
  observeEvent(
    input$coach,
    {
      shinyalert(
        title = NULL,
        text = paste0(
          '<table style = "width:100%; background-color:lightblue;">',
          '<tr>',
          '<td style = "width:35%;">',
          '<div>',
          '<img class = "myImg1" src = "head_coach.webp" height = "250px"></img>',
          '<h3>Gregg Popovich</h3></div>',
          '</td>',
          '<td style = "width:65%; text-align:left;">',
          '<h4><li>5x NBA champion as a coach</li></h4>',
          '<h4><li>4x NBA All-Star Game head coach</li></h4>',
          '<h4><li>3x NBA Coach of the Year</li></h4>',
          '</td>',
          '</tr>',
          '</table>',
          
          #Adding a table show the coach's other info
          '<table style="width:100%; background-color:lightblue;border-collapse: collapse; border: 1px solid black;">',
          '<tr>',
          '<th style="border: 2px solid black; padding: 5px; text-align: center;">Residence</th>',
          '<th style="border: 2px solid black; padding: 5px; text-align: center;">Birth Date</th>',
          '<th style="border: 2px solid black; padding: 5px; text-align: center;">Email</th>',
          '</tr>',
          '<tr>',
          paste0('<td style="border: 2px solid black; padding: 5px; text-align: center;">', coach$city, ', ', coach$country, '</td>',
                 '<td style="border: 2px solid black; padding: 5px; text-align: center;">', coach$dob, '</td>',
                 '<td style="border: 2px solid black; padding: 5px; text-align: center;">', coach$email, '</td>'),
          '</tr>',
          '</table>'
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # uiPlayers(2.2)----
  output$uiPlayers <- renderUI(
    {
      div(
        style = 'padding:50px 15% 0 15%;',
        align = 'center',
        h2(
          style = 'color:white;',
          'Players'
        ),
        lapply(
          0:2, 
          function(i) {
            fluidRow(
              style = 'padding:30px 0 0 0;',
              lapply(
                1:4, 
                function(j) {
                  k <- 4 * i + j
                  column(
                    width = 3,
                    img(
                      src = paste0('players/', k, '.webp'),
                      width = '125px',
                      height = '125px',
                      style = paste0(
                        'object-fit:cover; margin:0 10px 0 10px; ',
                        'border:solid white 2px; border-radius:50%;',
                        'background-color:lightblue;'
                      )
                    ),
                    br(), br(),
                    actionBttn(
                      inputId = paste0('plyr', k),
                      label = plyrs$last_name[k], plyrs$first_name[k],
                      style = 'unite',
                      color = 'primary',
                      size = 'xs',
                      block = TRUE
                    )
                  )
                }
              )
            )
          }
        )
      )
    }
  )
  
  #_event plyr button----
  observeEvent(input$plyr1, {pProf(1)})
  observeEvent(input$plyr2, {pProf(2)})
  observeEvent(input$plyr3, {pProf(3)})
  observeEvent(input$plyr4, {pProf(4)})
  observeEvent(input$plyr5, {pProf(5)})
  observeEvent(input$plyr6, {pProf(6)})
  observeEvent(input$plyr7, {pProf(7)})
  observeEvent(input$plyr8, {pProf(8)})
  observeEvent(input$plyr9, {pProf(9)})
  observeEvent(input$plyr10, {pProf(10)})
  observeEvent(input$plyr11, {pProf(11)})
  observeEvent(input$plyr12, {pProf(12)})
  
  #_prof function----
  pProf <- function(i) {
    shinyalert(
      title = NULL,
      text = paste0(
        '<table style = "width:100%; background-color:lightblue;">',
        '<tr>',
        '<td style = "width:35%;">',
        '<div>',
        '<img class = "myImg1" src = "players/', i, '.webp" height = "250px"></img>',
        '<h4>', plyrs$first_name[i], ' ', plyrs$last_name[i],'</h4></div>',
        '</td>',
        '<td style = "width:65%; text-align:left;">',
        '<h4>Height: ', plyrs$ht[i] %/% 12, '\'', plyrs$ht[i] %% 12, '"</h4>',
        '<h4>Weight: ', plyrs$wt[i], ' lbs</h4>',
        '<h4>Position: ', plyrs$pos[i], '</h4>',
        '<h4>Number: ', plyrs$player_number[i], '</h4>',
        '<hr>',
        '<h4>', plyrs$yr_exp[i], ' Years of Experience</h4>',
        '<h4>', plyrs$total_gms[i], ' Games Played</h4>',
        '<h4>', plyrs$all_star_exp[i], 'x NBA All-Star</h4>',
        '<h4>', plyrs$all_nba_honors[i], 'x All-NBA Honors</h4>',
        '<hr>',
        '</td>',
        '</tr>',
        '</table>',

        #Adding a table show players' key stats
        '<h3>Key Performance Metrics</h3>',
        '<table style="width:100%;border-collapse: collapse; border: 1px solid black;">',
        '<tr>',
        '<th style="border: 2px solid black; padding: 5px; text-align: center;">PPG</th>',
        '<th style="border: 2px solid black; padding: 5px; text-align: center;">RPG</th>',
        '<th style="border: 2px solid black; padding: 5px; text-align: center;">APG</th>',
        '<th style="border: 2px solid black; padding: 5px; text-align: center;">SPG</th>',
        '<th style="border: 2px solid black; padding: 5px; text-align: center;">BPG</th>',
        '</tr>',
        '<tr>',
        paste0('<td style="border: 2px solid black; padding: 5px; text-align: center;">',
               format(
                 round(
                   (plyrs$fg2[i] * 2 + plyrs$fg3[i] * 3 + plyrs$ft[i]),
                   1
                 ),
                 nsmall = 1
               ), '</td>'),
               '<td style="border: 2px solid black; padding: 5px; text-align: center;">', plyrs$reb[i], '</td>',
               '<td style="border: 2px solid black; padding: 5px; text-align: center;">', plyrs$ast[i], '</td>',
               '<td style="border: 2px solid black; padding: 5px; text-align: center;">', plyrs$stl[i], '</td>',
               '<td style="border: 2px solid black; padding: 5px; text-align: center;">', plyrs$blk[i], '</td>',
        '</tr>',
        '</table>'
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
  }
  
  
  # >>>>>>>>>>>>>>>>>>----
  # uiTickets(3.1)----
  output$uiTickets <- renderUI({
    div(
      style = 'padding:100px 15% 0 15%;',
      wellPanel(
        style = 'background-color:rgba(0,0,0,0.8);display: flex;',
        fluidRow(
          # Image column
          column(
            width = 6,
            img(
              src = 'stadium/rogers0.jpg',
              width = '140%',
              style = "display: block; margin: 0 auto;"
            )
          ),
          # Text and button column
          column(
            width = 6,
            style = 'display: flex; flex-direction: column; justify-content: center; height: 100%; padding-right: 20px;',
            tags$h2(tags$strong('Welcome to Rogers!'), style = 'text-align: right; color:white;'), 
            tags$a(
              href = 'https://ticketsonsale.com/venues/rogers-arena?gclid=Cj0KCQjwrMKmBhCJARIsAHuEAPTvh7bxnF4p1-vZ-8ILjJVaM_kIMmav_lYlFCdrziG1thLGnVaAKLoaApNREALw_wcB',
              target = '_blank',
              div(
                actionBttn(
                inputId = 'buyTix1',
                label = 'Buy Tickets',
                style = 'gradient',
                color = 'success',
                size = 'lg',
                block = FALSE
              ),
              style = 'margin-top: auto; text-align: right;padding-right: 50px;' 
              )
              )
          )
        )
      )
    )
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiEvents(3.2)----
  concerts <- data.frame(
                artist = c('Lionel Richie', 'Coldplay', 'Eason Chan', 'Tim McGraw'),
                name = c('All Night Long in Vancouver!', 'Sky Full of Stars Over Vancouver', 'East Meets West: Eason\'s Encore in Vancouver', 'Southern Sounds in the Heart of Vancouver'),
                date = c('September 8, 2023', 'October 15, 2023', 'November 12, 2023', 'December 5, 2023'),
                time = c('7:00pm', '8:30pm', '9:00pm', '6:30pm'),
                desc = c('Experience the magic and nostalgia of Lionel Richie live at Rogers Arena. From "Hello" to "All Night Long," relive the moments with the iconic star as he serenades Vancouver with his timeless hits.',
                          'Dive into a cosmic musical journey with Coldplay as they light up Rogers Arena with their ethereal melodies and anthems. Join us for an unforgettable evening and let\'s color the night sky together.',
                          'Eason Chan, the Prince of Canto-pop, returns to Vancouver with a mesmerizing performance that promises a blend of Eastern melodies and modern pop. Witness the harmonious blend of cultures in a musical evening to remember.',
                          'Country legend Tim McGraw brings the spirit of Nashville to Vancouver. Get ready to don your cowboy boots and sway to the heartfelt ballads and energetic hits that have defined a generation of country music lovers.'),
                link = c('https://www.ticketmaster.ca/lionel-richie-and-earth-wind-fire-vancouver-british-columbia-09-12-2023/event/11005E61C2183BB9?',
                         'https://www.stubhub.ca/coldplay-vancouver-tickets-9-22-2023/event/151413979/?quantity=2',
                         'https://www.stubhub.ca/eason-chan-vancouver-tickets-9-25-2023/event/152090633/?quantity=2',
                         'https://www.ticketmaster.ca/tim-mcgraw-standing-room-only-tour-vancouver-british-columbia-03-27-2024/event/11005EFB945C1864?_gl=1*1yf97kk*_gcl_au*MTE3NTA3MTkwMC4xNjkxNDUxMjU4*_ga*MTE1MDAxNTAwMy4xNjkxNDUxMjU5*_ga_H1KKSGW33X*MTY5MTQ1MTI1OS4xLjEuMTY5MTQ1MTM2NC42MC4wLjA.&_ga=2.212293014.307871460.1691451259-1150015003.1691451259')
              )
  
  observeEvent(input$Event1, {
    c$ev <- ifelse(c$ev > 1, c$ev - 1, 4)
  })
  
  observeEvent(input$Event2, {
    c$ev <- ifelse(c$ev < 4, c$ev + 1, 1)
  })
  
  output$uiEvents <- renderUI({
    div(
      style = 'padding:15px 20% 0 20%;',
      align = 'center',
      tags$h2(style = "text-align: center; padding:10px; text-shadow:2px 2px white;", 
              tags$strong("Rogers Arena Concert Series: Where Legends Meet the Spotlight!")),
      wellPanel(
        style = "background-color: rgba(255,255,255,0.5);padding:5px",
        fluidRow(
          # For concert name
          column(width = 12, style = "text-align: center;text-shadow:2px 2px white; padding:0px;",
                 h3(tags$strong(concerts$name[c$ev]))
          )
        ),
        fluidRow(
          # For date
          column(width = 4, style = "text-align: left;", 
                 h4(icon("calendar-alt", lib = "font-awesome"), 
                    HTML(paste0("<b>", concerts$date[c$ev], "</b>")))
          ),
          # For artist name
          column(width = 4, style = "text-align: center;", 
                 h4(icon("user", lib = "font-awesome"), 
                    HTML(paste0("<b>", concerts$artist[c$ev], "</b>")))
          ),
          # For time
          column(width = 4, style = "text-align: right;", 
                 h4(icon("clock", lib = "font-awesome"), 
                    HTML(paste0("<b>", concerts$time[c$ev], "</b>")))
          )
        ),
        fluidRow(
          # For description
          column(width = 12, style = "text-align: center;",
                 h4(concerts$desc[c$ev])
          )
        )
        ),
        wellPanel(
          tags$img(
            src = paste0('concert/', c$ev, '.gif'),
            width = '100%'
        )
        ),
        div(
          style = 'display: flex; justify-content: space-between;',
          actionBttn(
            inputId = 'Event1',
            label = '<-',
            style = 'jelly',
            color = 'warning',
            size = 'm',
            block = FALSE
        ),
        a(
          href = concerts$link[c$ev],
          target = "_blank",
          actionBttn(
            inputId = 'buyTix2',
            label = 'Buy Tickets',
            style = 'material-flat',
            color = 'warning',
            size = 'm',
            block = FALSE
          )
        ),
        actionBttn(
          inputId = 'Event2',
          label = '->',
          style = 'jelly',
          color = 'warning',
          size = 'm',
          block = FALSE
        )
      ),
      br(), br()
    )
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiGames(3.3)----
  output$uiGames <- renderUI(
    {
      div(
        style = 'padding:100px 15% 0 15%;',
        align = 'center',
        wellPanel(
          img(
            #lol1
            src = paste0('stadium/lol1.jpeg'),
            width = '80%'
          ),
          
          h3('League Of Legends 2024 World Championship'),
          
          #buy ticket
          tags$a(
            href = 'https://www.ticketsonsale.com/sports/league-of-legends/7?gclid=Cj0KCQjwldKmBhCCARIsAP-0rfwHDpZp7Iph5ixNggMO85ODUb4CX4BT2gcG3ILRBzlkejEta34C4JMaAtTLEALw_wcB',
            target = '_blank',
            
            actionBttn(
              inputId = 'buyTix3',
              label = 'Buy Tickets',
              style = 'gradient',
              color = 'success',
              size = 'lg',
              block = FALSE
            )
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # uiArena(3.4)----
  output$uiArena <- renderUI({
    div(
      style = 'padding:50px 5%;',
      fluidRow(
        column(width = 12,
               h1("Rogers Arena - Vancouver", 
                  style = "text-align:center; color:darkblue; margin-bottom:50px;")
        ),
        
        # Information column
        column(
          width = 6,
          div(
            style = 'padding:20px;display:flex; align-items:center; height: 100%;',
            wellPanel(
              align = 'left',
              style = 'background-color:rgba(250,250,250,0.7); padding:20px; border-radius: 10px;',
              tags$img(src = "stadium/rog.jpg", width = "400px", style = "display: block; margin: auto;"),
              h3(
                tags$strong('Rogers Arena - An Overview'), 
                style="color:darkblue; margin-bottom:10px;text-align:center;"
              ),
              tags$p(
                style= "color:black;font-size:18px;text-align:center;",
                'Rogers Arena is a popular multi-purpose indoor arena located in Vancouver, Canada. ',
                'It hosts a diverse range of events such as concerts, sporting events, shows, and conferences. ',
                'The specific events held at Rogers Arena each year vary, ',
                'dependent upon the schedule set forth by arena management and event organizers.'
              ),
              hr(style="border-top: 1px solid gray;"),
              # Address
              tags$p('800 Griffiths Way, Vancouver, BC V6B 6G1, Canada', style="font-weight: bold; color:gray;text-align:center;")
            )
          )
        ),
        # Map column
        column(
          width = 6,
          wellPanel(
            style = 'background-color:rgba(250,250,250,0.7); padding:10px; border-radius: 10px;',
            leafletOutput('mapArena', height = '75vh')
          )
        )
      )
    )
  })
  
  #_mapArena----
  output$mapArena <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addAwesomeMarkers(
          lat = ra[1],
          lng = ra[2]
        )
    })

  # >>>>>>>>>>>>>>>>>>----
  # uiMuseum(4.1)----
  museums <- data.frame(
    name = c("Science World", "Bill Reid Gallery of Northwest Coast Art", "Beaty Biodiversity Museum",
             "Museum of Anthropology at UBC", "Gulf of Georgia Cannery", "The Polygon Gallery",
             "Vancouver Art Gallery", "Fort Langley National Historic Site", "Vancouver Police Museum",
             "Rennie Museum"),
    link = c("https://www.cntraveler.com/activities/vancouver/science-world",
             "https://www.cntraveler.com/activities/vancouver/bill-reid-gallery-of-northwest-coast-art",
             "https://www.cntraveler.com/activities/vancouver/beaty-biodiversity-museum",
             "https://www.cntraveler.com/activities/vancouver/museum-of-anthropology-at-ubc",
             "https://www.cntraveler.com/activities/vancouver/gulf-of-georgia-cannery",
             "https://www.cntraveler.com/activities/vancouver/the-polygon-gallery",
             "https://www.cntraveler.com/activities/vancouver/vancouver-art-gallery",
             "https://www.cntraveler.com/activities/vancouver/fort-langley-national-historic-site",
             "https://www.cntraveler.com/activities/vancouver/vancouver-police-museum",
             "https://www.cntraveler.com/activities/vancouver/rennie-museum"),
    image_filename = c("m1.jpg", "m2.jpg", "m3.jpg", "m4.jpg", "m5.jpg",
                       "m6.jpg", "m7.jpg", "m8.jpg", "m9.jpg", "m10.jpg")
  )
  
  output$uiVAN1 <- renderUI({
    i <- currentMuseum()
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; flex-direction: column; justify-content: center; height: 100vh;",
          
          # Introduction container with background image
          div(
            style = "background-image: url('van_art.jpg'); 
                   background-size: cover; 
                   background-position: center;
                   padding: 20px 0;  # Adjusted padding here
                   text-align: center; 
                   color: white; 
                   display: flex; 
                   flex-direction: column; 
                   justify-content: center;",
            
            div(
              style = "background-color: rgba(0, 0, 0, 0.6); padding: 20px 40px; max-width: 900px; margin: auto;",
              tags$p(
                "Set against stunning mountains and ocean, Vancouver is a blend of rich history and diverse culture. Here, history isn't just told; it's felt!",
                style = "text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.7); font-size: 16px;text-align:center;"
              )
            )
          ),
          
          # Displaying museum image
          div(
            style = 'text-align: center;',
            tags$a(
              tags$img(
                src = museums$image_filename[i],
                width = "650px",
                height = "500px",
                style = "border: 1px solid orange; margin: 10px; padding: 30px;
                       border-radius: 10px;object-fit: cover;opacity: 1;"
              ),
              style = "display: block;",
              href = museums$link[i],
              target = "_blank"
            ),
            h3(
              tags$strong(museums$name[i]), 
              style = "font-size: 20px; white-space: nowrap;margin-top: -30px;"
            ),
            fluidRow(
              column(4, offset = 4,
                     div(style = "margin-top: 10px; display: flex; justify-content: space-between;",
                         actionButton("prevBtn", "Previous", style = "width: 48%; background-color: lightsalmon;"),
                         actionButton("nextBtn", "Next", style = "width: 48%; background-color: lightsalmon;")
                     )
                     
              )
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$nextBtn, {
    # Increment the current museum index when the "Next" button is clicked
    if (currentMuseum() < nrow(museums)) {
      currentMuseum(currentMuseum() + 1)
    } else {
      currentMuseum(1) # Loop back to the first museum when reaching the last museum
    }
  })
  
  observeEvent(input$prevBtn, {
    # Decrement the current museum index when the "Previous" button is clicked
    if (currentMuseum() > 1) {
      currentMuseum(currentMuseum() - 1)
    } else {
      currentMuseum(nrow(museums)) # Loop back to the last museum when reaching the first museum
    }
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiRoute(4.2)----
  
  #_Tour Package content----
  output$tour_content <- renderUI({
    req(input$selected_routes)
    if ("Tour Package" %in% input$selected_routes) {
      tagList(
        fluidRow(
          column(
            width = 6,
            wellPanel(
              img(src = "tour1.jpg", width = "100%"),
              h5("Vancouver Seaplane Tour", style = "font-weight: bold; font-size: 18px; text-align: center;"),
              a(href = "https://www.tripadvisor.com/AttractionProductReview-g154943-d11448261-Vancouver_Seaplane_Tour-Vancouver_British_Columbia.html", target = "_blank",
                img(src = "tripadvisor.png", width = "30px", style = "display: block; margin: auto;")
              )
            )
          ),
          column(
            width = 6,
            wellPanel(
              img(src = "tour2.jpg", width = "100%", id = "PanelImage2"),
              h5("Stanley Park Bike Tour", style = "font-weight: bold; font-size: 18px; text-align: center;"),
              a(href = "https://www.tripadvisor.com/AttractionProductReview-g154943-d11455120-Stanley_Park_Bike_Tour-Vancouver_British_Columbia.html", target = "_blank",
                img(src = "tripadvisor.png", width = "30px", style = "display: block; margin: auto;")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              img(src = "tour3.jpg", width = "100%"),
              h5("Gastown Historic Walking Food Tour", style = "font-weight: bold; font-size: 18px; text-align: center;"),
              a(href = "https://www.tripadvisor.com/AttractionProductReview-g154943-d11457968-Gastown_Historic_Walking_Food_Tour-Vancouver_British_Columbia.html", target = "_blank",
                img(src = "tripadvisor.png", width = "30px", style = "display: block; margin: auto;")
              )
            )
          ),
          column(
            width = 6,
            wellPanel(
              img(src = "tour4.jpg", width = "100%"),
              h5("Vancouver Harbor Sunset Dinner Cruise", style = "font-weight: bold; font-size: 18px; text-align: center;"),
              a(href = "https://www.tripadvisor.com/AttractionProductReview-g154943-d12000832-Vancouver_Harbor_Sunset_Dinner_Cruise-Vancouver_British_Columbia.html", target = "_blank",
                img(src = "tripadvisor.png", width = "30px", style = "display: block; margin: auto;")
              )
            )
          )
        )
      )
    }
  })
  
  #_City Walk content----
  output$walk_content <- renderUI({
    req(input$selected_routes)
    if ("City Walk" %in% input$selected_routes) {
      fluidRow(
        column(
          width = 6,
          wellPanel(
            img(src = "route1.jpg", width = "100%"),
            h5("Seawall Water Walk", style = "font-weight: bold; font-size: 18px; text-align: center;"),
            div(
              style = "height: 160px;",
              tags$p("Explore the world's longest uninterrupted waterfront pathway - the Vancouver Seawall. This iconic 28-kilometer trail encircles Vancouver, offering stunning views of mountains, ocean, and cityscape. A must-visit for nature lovers and urban adventurers alike!", style = "text-align: center;")
            )
          )
        ),
        column(
          width = 6,
          wellPanel(
            img(src = "route2.jpg", width = "100%"),
            h5("Butchart Garden", style = "font-weight: bold; font-size: 18px; text-align: center;"),
            div(
              style = "height: 160px;",
              tags$p("Step into Butchart Gardens, Vancouver's enchanting oasis of floral splendor. Set against the serene backdrop of British Columbia's landscape, this world-class garden offers a mesmerizing tapestry of vibrant blooms, serene water features, and intricate garden designs. A visit promises a delightful escape that captivates the senses, making it an unmissable gem for both nature aficionados and those seeking a picturesque retreat.", style = "text-align: center;"),
            )
          )
        )
      )
    }
  })
  
  # Dynamically render the appropriate UI based on the selected tab
  output$uiVAN2 <- renderUI({
    fluidPage(
      tags$head(
        tags$style(
          HTML("
          .container {
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
          }
          .sidebar {
            margin-top: 50px;
          }
          #PanelImage2 {
            height: 252px;
          }
          ")
        )
      ),
      div(
        class = "container",
        sidebarPanel(
          style = "background-color:#FFDAB9;",
          h4("Select Your Interested Routes!", style = "white-space: nowrap; font-size: 15px; font-weight: bold;"),
          radioButtons("selected_routes", "Routes", choices = c("Tour Package", "City Walk"))
        ),
        div(class = "sidebar", uiOutput("tour_content")),
        div(class = "sidebar", uiOutput("walk_content"))
      )
    )
  })
 
 # >>>>>>>>>>>>>>>>>>----
 # uiRestaurant section(4.3)----
  
  restaurants <- data.frame(
    rest_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    name = c("Five Sails Restaurant",
             "Teahouse in Stanley Park",
             "Cardero's Restaurant & Live Bait Marine Pub",
             "The Sandbar Seafood Restaurant",
             "Prospect Point Bar & Grill",
             "Cactus Club Cafe English Bay",
             "Water St. Cafe", "Salmon n' Bannock Bistro",
             "Bao Bei Chinese Brasserie","Brix & Mortar Restaurant"),
    lat = c(49.287691, 49.3011652, 49.2916136, 49.272198, 49.3125995, 49.2867426, 49.2841146, 49.2633065, 49.2797504, 49.2756158),
    lng = c(-123.1157624, -123.1596816, -123.1300565, -123.133965, -123.145255, -123.1452999, -123.1116888, -123.1323446, -123.103167,-123.1244854),
    price_level = c("3", "4", "2", "3", "3", "4", "2","2", "3","4")
  )
  
  output$uiVAN3 <- renderUI({
      div(
        # Create a container for the map
        div(
          style = "padding-top:50px; display: flex; justify-content: center;",
          div(
            class = "container",
            fluidRow(
              column(
                width = 12,
                leafletOutput(
                  outputId = 'map',
                  width = "100%",
                  height = "400px"
                )
              )
            )
          )
        ),
        # Create a container for the restaurant list and options
        div(
          style = "padding-top:20px;display: flex; justify-content: center; width: 100%;",
          fluidRow(
            column(
              width = 12,
              align = 'center',
              uiOutput('restList')
            ),
            column(
              width = 12,
              uiOutput('restOpts')
            )
          )
        )
      )
  })
  
  #_ui restmap----
  output$map <- renderLeaflet({
    leaflet(data = restaurants) %>%
      addTiles() %>%
      addMarkers(
        data = restaurants,
        lat = ~lat,
        lng = ~lng,
        popup = ~name,
  )
  })
  
  #_ui list---- 
  output$restList <- renderUI({
    tags$div(
      style = "max-height: 400px; overflow-y: auto;",
      lapply(1:10, function(i) {
        wellPanel(
          fluidRow(
            column(
              width = 8,
              align = 'left',
              h4(restaurants$name[i]),
              h5(
                tags$span(
                  style = 'color:orange;',
                  paste0(
                    rep('$', restaurants$price_level[i]),
                    collapse = ''
                  )
                )
              )
            ),
            column(
              width = 4,
              actionBttn(
                inputId = paste0('rest_id', i),
                label = 'Reserve Here',
                style = 'simple',
                color = 'success'
              )
            )
          )
        )
      })
    )
  })
  
  # react to map click
  observeEvent(input$map_marker_click,
               { x = restaurants %>%
                 filter(lat == input$map_marker_click$lat,
                        lng == input$map_marker_click$lng)
               
               leafletProxy(
                 mapId = 'map'
               ) |>
                 flyTo(
                   lat = input$map_marker_click$lat,
                   lng = input$map_marker_click$lng,
                   zoom = 10,
                   options = list(duration = 2)
                 ) |> 
                 clearPopups() |> 
                 addPopups(
                   lat = input$map_marker_click$lat,
                   lng = input$map_marker_click$lng,
                   popup = paste0(
                     '<style>div.leaflet-popup-content {width:auto !important;}</style>',
                     '<div style = "overflow:hidden; display:flex; ',
                     'align-items:center; justify-content:center; ',
                     'width:400px; max-height:300px;">',
                     '<img src = "r',x$rest_id,'.jpg" ',
                     'width = "250px" ',
                     'style = "border-radius:5px;"></img></div>'
                   )
                 )
               })
  
  #_function for map zoom in----
  flytorest <- function() {
    rest = restaurants
    leafletProxy(mapId = 'map') %>% 
      flyTo(
        lat = rest$lat[a$r],
        lng = rest$lng[a$r],
        zoom = 17
      )}
      
  #_reserve form----
  reserveModal <- function() {
    modalDialog(
      title = "Make a Reservation",
      tags$head(tags$style(HTML("
          .shiny-options-group {
                display: flex;
                justify-content: center;
                width: 100%;
          }
      "))),
      dateInput(
        inputId = 'restDate',
        label = 'Date',
        value = Sys.Date(),
        min = Sys.Date(),
        width = '80%'
      ),
      pickerInput(
        inputId = 'restTime',
        label = 'Time',
        choices = paste0(c(12:23), ':00'),
        selected = '12:00',
        width = '80%',
        inline = FALSE
      ),
      radioGroupButtons(
        inputId = 'restSize',
        label = 'Party Size',
        choices = c(1:6),
        selected = 2
      ),
      actionBttn(
        inputId = 'restConf',
        label = 'Confirm Booking',
        style = 'gradient',
        color = 'primary'
      ),
      footer = tagList(
        actionButton("closeModal", "Close") 
      )
    )}
  
  observeEvent(input$closeModal, {
    removeModal()
  })
  
  # Loop through 1 to 10 to create observers for each button
  lapply(1:10, function(i) {
    observeEvent(input[[paste0("rest_id", i)]], {
      a$r <- i
      flytorest()
      showModal(reserveModal())
    })
  })
  
  # _event rest conf button----
  observeEvent(input$restConf, {
      sendSweetAlert(
        session = session,
        title = 'Thank you!',
        text = paste0(
          'Your reservation for a party of ',
          input$restSize,
          ' at ',
          restaurants$name[a$r], 
          ' is confirmed. See you on ',
          input$restDate,
          ' at ',
          input$restTime, '!'
        )
      )
    })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiMall(4.4)----
  mall <- data.frame(
    name = c("CF Pacific Center", "Metrotwon", "Granville Island Market", "Robson Street", "Hudson's Bay" ),
    link = c("https://shops.cadillacfairview.com/property/cf-pacific-centre",
             "https://www.metropolisatmetrotown.com/",
             "https://granvilleisland.com/public-market",
             "https://robsonstreet.ca/",
             "https://locations.thebay.com/en/hudsons-bay-vancouver-downtown"
    ),
    image_filename = c("cf.jpg", "metro.jpg", "GIM.jpg","robson.jpg", "hudson.jpg"),
    description = c("Experience luxurious shopping and premium retailers at CF Pacific Centre in the heart of downtown, featuring Holt Renfrew, Canada Goose, Harry Rosen, Hugo Boss, Kate Spade New York and many more. Unlock Exclusive Tourist offers by visiting guest services!"
                    ,
                    "Burnaby‘s Metropolis at Metrotown is Vancouver’s largest mall. The centre boasts a large state-of-the-art food court and 330 quality brand name stores such as Apple, Aritzia, Banana Republic, Indigo, Michael Kors, Microsoft, Muji, Nespresso, Sephora, Sport Chek, Hudson's Bay, Uniqlo, Victoria's Secret, Winners/Homesense and Zara. Whether you are looking for the latest designer bag, to catch a movie or for a last-minute gift, Metropolis at Metrotown is the perfect destination to find everything. "
                    , 
                    "Built in 1979, the market is one of Vancouver’s most popular tourist attractions. It’s a great place to buy high quality fresh produce, as well as other tasty food products. The market is a fascinating place and well worth checking out! Types of food available for purchase at the Granville Island Public Market include ethnic foods, fresh produce, gourmet cheeses, butcher shop-style meats, fresh seafood, exotic spices and more."
                    ,
                    "Robson Street is a must-stroll for most visitors to Vancouver. One of Vancouver’s oldest commercial streets, it was once known as Robsonstrasse for the sheer number of German and European stores that opened up after the Second World War. The international character of the street still exists. Being right in the heart of the downtown core, you’ll find yourself walking right alongside locals on their way to the office, sports fans heading to the game, and students lining up outside noodle shops."
                    ,
                    "The Bay Building is a six-story building on the corner of Granville Street and Georgia Street in downtown Vancouver, British Columbia, Canada. It is a flagship store of the Hudson's Bay department store chain. It features a toy department, premium designer boutique The Room, and one of our HBC Signature Shops, where you’ll find a greater selection of our iconic striped merchandise including Point Blankets. You’ll also find the biggest TOPSHOP TOPMAN in all of Canada and a 10,000-square-foot athletic shop for men and women."),
    rating = c ('4','4','4.5','4','4') #according to Trip Advisor
  )  

  observeEvent(input$mall1, {
    s$ev <- ifelse(s$ev > 1, s$ev - 1, nrow(mall))
  })
  
  observeEvent(input$mall2, {
    s$ev <- ifelse(s$ev < nrow(mall), s$ev + 1, 1)
  })
  
  output$uiVAN4 <- renderUI({
    div(
      style = "padding-top: 30px;",
      align = "center",
      h2(style = "color: white;", "Dive into Vancouver's Premier Shopping Havens!"),
      wellPanel(
        style = "background-color: rgba(0, 0, 0, 0.5);max-width: 800px; margin: 0 auto;",
        tags$img(
          src = mall$image_filename[s$ev],
          width = '600px',
          height = '500px'
        ),
        h2(style = "color:white;font-weight: bold;",
           mall$name[s$ev]
        ),
        h3(style = "color: white;", paste0('Rating: ', mall$rating[s$ev])),
        h4(style = "color: white;", mall$description[s$ev]),
        tags$a(href = "https://www.yourwebsite.com", target = "_blank",
               tags$button(class = "btn btn-success", "Visit Our Website")
        ),
        br(), br(),
        actionBttn(
          inputId = 'mall1',
          label = '<',
          style = 'material-circle',
          color = 'default',
          size = 'sm',
          block = FALSE
        ),
        actionBttn(
          inputId = 'mall2',
          label = '>',
          style = 'material-circle',
          color = 'default',
          size = 'sm',
          block = FALSE
        )
      )
    )
  })
  
  
  # >>>>>>>>>>>>>>>>>>----
  # uiHotel(4.5)----
  hotel <- data.frame(
                    name = c('Fairmont Hotel', 'Rosewood Hotel Georgia', 'The Sutton Place Hotel', 'Hyatt Regency Vancouver', 'Shangri-La Vancouver'),
                    lat = c(49.2837, 49.2835, 49.2825, 49.2853, 49.2858),
                    lng = c(-123.1211, -123.1190, -123.12428, -123.1208, -123.1236),
                    rating = c(4.5, 4.6, 4.3, 4.4, 4.6),
                    website = c('https://www.fairmont.com/hotel-vancouver/',
                                'https://www.rosewoodhotels.com/en/hotel-georgia-vancouver',
                                'https://www.suttonplace.com/vancouver',
                                'https://www.hyatt.com/en-US/hotel/canada/hyatt-regency-vancouver/yvrrv',
                                'https://www.shangri-la.com/en/vancouver/shangrila/'),
                    desc = c('Experience Fairmont Vancouver where luxury meets history, nestled in the city\'s heart, with unparalleled elegance and charm',
                                    'Downtown Vancouver\'s iconic blend of vintage grandeur, modern elegance, and coastal luxury',
                                    'Vancouver\'s sophisticated oasis, where timeless charm meets modern comforts in the city\'s heart',
                                    'Urban elegance meets coastal beauty, a centerpiece of luxury in Vancouver\'s vibrant downtown',
                                    'A sanctuary of opulence, blending urban sophistication with Asian serenity in the heart of Vancouver'
                                  )
  )
  
  output$uiVAN5 <- renderUI({
      div(
        style = 'padding:30px 10% 0 10%;',
        align = 'center',
        fluidRow(
          column(
            style = 'padding:15px;',
            width = 12,
            leafletOutput(
              outputId = 'hotelMap',
              height = '40vh'
            )
          ),
          column(
            width = 12,
            uiOutput('hotelOpts')
          ),
          column(
            width = 12,
            div(
              style = 'max-height: 800px; overflow-y: scroll;',
              uiOutput('hotelList')
            )
          )
        )
      )
    })
  
  # _hotel map----
  output$hotelMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addAwesomeMarkers(
        data = hotel,
        lat = ~lat,
        lng = ~lng,
        popup = ~name,
        icon = awesomeIcons(
          markerColor = 'blue',
          text = 1:nrow(hotel)
        )
      )
  })
  
  selected_hotel <- reactiveVal(1)
  
  #_hotel map function----
  flytohotel <- function() {
    hotel
    leafletProxy(mapId = 'hotelMap') %>% 
      flyTo(
        lat = hotel$lat[selected_hotel()],
        lng = hotel$lng[selected_hotel()],
        zoom = 15
      )}
  
  # _events hotel buttons----
  showForm <- reactiveVal(FALSE)

  prevButtonValues <- reactiveVal(rep(0, nrow(hotel)))

  observe({
    currentValues <- sapply(1:nrow(hotel), function(j) input[[paste0("hotel", j)]])
    previousValues <- prevButtonValues()
    
    for (i in 1:nrow(hotel)) {
      if (is.numeric(currentValues[i]) && is.numeric(previousValues[i])) {
        if (currentValues[i] > previousValues[i]) {
          selected_hotel(i)
          flytohotel()
          showForm(TRUE)
        }
      }
    }
    prevButtonValues(currentValues)
  })
  
  
  #_ui hotelList----
  output$hotelList <- renderUI({
    lapply(1:nrow(hotel), function(i) {
      is_partner_hotel <- hotel$name[i] == "Fairmont Hotel"
      background_style <- ifelse(is_partner_hotel, "background-color: lightblue;", "")
      
      # Define the partner message beforehand
      partner_message <- NULL
      if (is_partner_hotel) {
        partner_message <- h5(style = "color: darkgreen; font-weight: bold;", 
                              "Stay with Champions: Fairmont Hotel, the Official Partner of our Team!")
      }
      wellPanel(
        style = background_style,
        fluidRow(
          column(
            width = 4,
            tags$img(src = paste0('h', i, ".jpg"),
                     width = "100%",
                     onclick = paste0("Shiny.setInputValue('hotel", i, "', true);"))
          ),
          column(
            width = 5,
            align = 'left',
            h4(hotel$name[i]),
            partner_message, # Use the pre-defined partner message here
            h4(tags$span(paste0('Rating: ', hotel$rating[i]))),
            h4(hotel$desc[i]),
            h5(tags$a(href = hotel$website[i], target = "_blank", 'Website'))
          ),
          column(
            width = 3,
            actionBttn(
              inputId = paste0('hotel', i),
              label = 'Reserve Here',
              style = 'simple',
              color = 'success'
            )
          )
        )
      )
    })
  })
  
  
  # _events hotel scroll----
  observe({
    if (!is.null(selected_hotel())) {
      runjs('window.scrollTo({ top: 420, behavior: "smooth" });')
    }
  })
  
  # _ui hotel opts----
  output$hotelOpts <- renderUI({
    if (isTRUE(showForm())) {
      hotel_name <- hotel$name[selected_hotel()]
      
      wellPanel(
        tags$h4(
          style = 'text-align: center;',
          paste0('Reserve at ', hotel_name)
        ),
        
        fluidRow(
          column(width = 6,
                 dateInput(
                   inputId = 'hotelCheckInDate',
                   label = 'Check-In Date',
                   value = Sys.Date(),
                   min = Sys.Date(),
                   width = '70%'
                 )
          ),
          column(width = 6,
                 dateInput(
                   inputId = 'hotelCheckOutDate',
                   label = 'Check-Out Date',
                   value = Sys.Date() + 1,
                   min = Sys.Date() + 1,
                   width = '70%'
                 )
          )
        ),
        fluidRow(
          column(width = 6,
                 pickerInput(
                   inputId = 'hotelCheckInTime',
                   label = 'Check-In Time',
                   choices = paste0(c(12:23), ':00'),
                   selected = '12:00',
                   width = '90%',
                   inline = FALSE
                 )
          ),
          column(width = 6,
                 pickerInput(
                   inputId = 'hotelCheckOutTime',
                   label = 'Check-Out Time',
                   choices = paste0(c(08:13), ':00'),
                   selected = '10:00',
                   width = '90%',
                   inline = FALSE
                 )
          )
        ),
        radioGroupButtons(
          inputId = 'numGuests',
          label = 'Number of Guests',
          choices = c(1:6),
          selected = 2
        ),
        
        tags$div(
          style = 'text-align: center;',
          column(width = 12,
                 actionBttn(
                   inputId = 'confirmButton',
                   label = 'Confirm',
                   style = 'gradient',
                   color = 'success'
                 )
          )
        )
      )
    }
  })
  
  # _event for the confirm button----
  observeEvent(input$confirmButton, {
    sendSweetAlert(
      title = "Booking Successful!",
      text = paste0(
        'Your booking for ',
        input$numGuests,
        ' guests at ',
        hotel$name[selected_hotel()],
        ' is confirmed. See you soon',
        input$hotelCheckInDate,
        ' at ',
        input$hotelCheckInTime, '! '),
      type = "success"
    )
    
    showForm(FALSE)
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiNewsletter(5.1)----
  output$uiNewsletter <- renderUI(
    {
      div(
        style = 'padding:60px 10% 0 10%; background-color: rgba(255, 255, 255, 0.7);',
        align = 'left',
        h1(
          style = 'color:navyblue; text-shadow: 3px 3px 5px rgba(250,250,250,0.5);',
          'Insider Newsletter'
        ),
        h3(
          paste0(
            'Sign up for the Aviators Insider Newsletter and ',
            'receive exclusive access to Vancouver Aviators offers, ',
            'news, and more!'
          )
        ),
        hr(),
        fluidRow(
          # Fan profile----
          column(
            width = 6,
            h4(style = 'color:navyblue; text-align:center; text-shadow: 1px 1px 1px rgba(250,250,250,0.5);',
              tags$strong('Vancouver Fan Profile')),
            wellPanel(
              fluidRow(
                column(
                  width = 6,
                  align = 'left',
                  h5('Email Address')
                ),
                column(
                  width = 6,
                  textInput(
                    inputId = 'fanEmail',
                    label = NULL,
                    value = NULL,
                    width = '100%'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = 'left',
                  h5('Name')
                ),
                column(
                  width = 6,
                  textInput(
                    inputId = 'fanName',
                    label = NULL,
                    value = NULL,
                    width = '100%'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = 'left',
                  h5('Date of Birth')
                ),
                column(
                  width = 6,
                  dateInput(
                    inputId = 'fanDob',
                    label = NULL,
                    value = Sys.Date(),
                    width = '100%'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = 'left',
                  h5('Phone')
                ),
                column(
                  width = 6,
                  textInput(
                    inputId = 'fanPhone',
                    label = NULL,
                    value = NULL,
                    width = '100%'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  align = 'left',
                  h5('Zip Code')
                ),
                column(
                  width = 6,
                  textInput(
                    inputId = 'fanZip',
                    label = NULL,
                    value = NULL,
                    width = '100%'
                  )
                )
              )
            ),
            hr(),
            h4(tags$strong('Interests')),
            h5(
              paste0(
                'Which opposing team do you most enjoy ',
                'watching the Aviators take on?'
              ),
            ),
            pickerInput(
              inputId = 'oppTeam',
              label = NULL,
              choices = nbaTeams,
              selected = NULL
            ),
            h5(
              paste0(
                'Who is your favorite Aviators player?'
              ),
            ),
            pickerInput(
              inputId = 'favPlyr',
              label = NULL,
              choices = paste(plyrs$first_name, plyrs$last_name),
              selected = NULL
            ),
            h5(
              paste0(
                'Which of the following types of tickets ',
                'are you interested in?'
              ),
              radioGroupButtons(
                inputId = 'tixTypes',
                label = NULL,
                choices = c(
                  'Single Game',
                  'Full Season',
                  'Group Ticket'
                ),
                selected = NULL
              )
            )
          ),
          # _right side----
          column(
            width = 6,
            h4(style = 'color:navyblue; text-align:center; text-shadow: 1px 1px 1px rgba(250,250,250,0.5);',
               tags$strong('Preferences')),
            wellPanel(
              h5(
                paste0(
                  'Would you like to receive notifications ',
                  'for the following events?'
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  actionBttn(
                    inputId = 'aviaInsidr',
                    label = 'Aviators Insider',
                    style = 'minimal',
                    color = 'primary',
                    size = 'lg',
                    block = TRUE
                  ),
                  h6(
                    'Monthly content and special announcements'
                  )
                ),
                column(
                  width = 6,
                  radioGroupButtons(
                    inputId = 'aviaInsidrOpts',
                    label = NULL,
                    choices = c(
                      'Yes please',
                      'No thanks'
                    ),
                    selected = NULL,
                    direction = 'vertical',
                    justified = TRUE
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  actionBttn(
                    inputId = 'aviaTix',
                    label = 'Aviators Ticket Offers',
                    style = 'minimal',
                    color = 'primary',
                    size = 'lg',
                    block = TRUE
                  ),
                  h6(
                    'Exclusive offers on first-come, first-serve basis'
                  )
                ),
                column(
                  width = 6,
                  radioGroupButtons(
                    inputId = 'aviaTixOpts',
                    label = NULL,
                    choices = c(
                      'Yes please',
                      'No thanks'
                    ),
                    selected = NULL,
                    direction = 'vertical',
                    justified = TRUE
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  actionBttn(
                    inputId = 'aviaGuide',
                    label = 'Aviators Game Guide',
                    style = 'minimal',
                    color = 'primary',
                    size = 'lg',
                    block = TRUE
                  ),
                  h6(
                    'Receive game day information'
                  )
                ),
                column(
                  width = 6,
                  radioGroupButtons(
                    inputId = 'aviaInsidrOpts',
                    label = NULL,
                    choices = c(
                      'Yes please',
                      'No thanks'
                    ),
                    selected = NULL,
                    direction = 'vertical',
                    justified = TRUE
                  )
                )
              )
            )
          )
        )
      )
    })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiMobile app(5.2)----
  output$uiMobile <- renderUI(
    {
      div(
        style = 'padding:60px 10% 20px 10%;background-color: rgba(255, 255, 255, 0.7);',
        align = 'left',
        h1(
          style = 'color:navyblue; text-shadow: 3px 3px white rgba(250,250,250,0.5);',
          'Mobile App'
        ),
        hr(),
        h3(
          style = 'text-shadow: 2px 2px white rgba(250,250,250,0.5);',
          tags$strong('Download the mobile app and keep up to date with stats, live scores, news, player profiles, and more!'),
        ),
        h4(
          style = 'font-size: 18px;',
          'Features Included:'
        ),
        tags$ul(
          tags$li(
            h5(
              style = 'font-size: 15px;',
              'Easier switch between Vancouver Aviators and Rogers Arena for up-to-date info on all events in Rogers Arena.',
            )
          ),
          tags$li(
            h5(style = 'font-size: 15px;','View team schedule on interactive team calendar.')
          ),
          tags$li(  
            h5(style = 'font-size: 15px;','Receive push notifications.')
          ),
          tags$li(
            h5(style = 'font-size: 15px;','Compete in fan competition and win prizes!')
          ),
          tags$li(
            h5(style = 'font-size: 15px;','View roster breakdowns, player bios, stats, and photos')
          ),
          tags$li(
            h5(style = 'font-size: 15px;','Post content directly to your social media accounts from the app')
          ),
          tags$li(
            style = 'font-size: 15px;',
            h5('Access to Aviators Promotions, including tickets merchandise, concessions, and more')
          )
        ),
        hr(),
        actionBttn(
          inputId = 'appApple',
          label = 'Download on the Apple Store',
          style = 'jelly',
          color = 'danger',
          size = 'lg'
        ),
        actionBttn(
          inputId = 'appGoogle',
          label = 'Get it on Google Play',
          style = 'jelly',
          color = 'primary',
          size = 'lg'
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # ui fan events(5.3)----
  output$uiFanEvents <- renderUI({
    div(
      style = 'padding:60px 10% 0 10%;',
      h1(
        style = 'color:black; text-align:center; text-shadow: white 2px 2px;',
        'Fan Events'
      ),
      # First Event
      wellPanel(
        fluidRow(
          column(
            width = 3, 
            tags$img(src = 'event1.jpg', height = 200, width = "100%")
          ),
          column(
            width = 3,
            actionBttn(
              inputId = 'fanEvent1',
              label = 'Youth Basketball Camp',
              style = 'minimal',
              color = 'primary',
              size = 'lg',
              block = TRUE
            )
          ),
          column(
            width = 6,
            h5(style = 'font-size: 15px; color: grey; line-height: 1.5;',
              'The Vancouver Aviators are proud to bring basketball to youth across not only the city, but also Western Canada. We also provide the opportunities for the kids to learn basketball skills from our star players, including Jayson Tatum and Kyrie Irving! We hope to see you soon!'
            )
          )
        )
      ),
      
      # Second Event
      wellPanel(
        fluidRow(
          column(
            width = 3,
            tags$img(src = 'event2.jpg', height = 200, width = "100%")
          ),
          column(
            width = 3,
            actionBttn(
              inputId = 'fanEvent2',
              label = 'NBA Legends Meet & Greet',
              style = 'minimal',
              color = 'primary',
              size = 'lg',
              block = TRUE
            )
          ),
          column(
            width = 6,
            h5(style = 'font-size: 15px; color: grey; line-height: 1.5;',
              'We have NBA legend appearances where fans can get the opportunity to pose for pictures, get an autograph and more!'
            )
          )
        )
      ),
      
      # Third Event
      wellPanel(
        fluidRow(
          column(
            width = 3,
            tags$img(src = 'event3.jpg', height = 200, width = "100%")
          ),
          column(
            width = 3,
            actionBttn(
              inputId = 'fanEvent3',
              label = 'More Community Fan Experience',
              style = 'minimal',
              color = 'primary',
              size = 'lg',
              block = TRUE
            )
          ),
          column(
            width = 6,
            h5(style = 'font-size: 15px; color: grey; line-height: 1.5;',
              'The Vancouver Aviators have created more exclusive experiences for fans to participate in throughout the season. More information on experience packages to follow. All proceeds generated from fan experience will be donated back to the Vancouver community.'
            )
          )
        )
      )
    )
  })
  
  
  # >>>>>>>>>>>>>>>>>>----
  #_uiShop(5.4)----
  # Product Data
  product <- data.frame(
    product_id = 1:4,
    name = c('Vancouver Aviators T Shirt', 'Nike x Aviators Socks', 'Souvenir Edition Mugs', 'Nike x Aviators Basketball Cap'),
    img = c('good1.png', 'good2.png', 'good3.png', 'good4.png'),
    price = c(70, 20, 20, 50)
  )
  
  # Initialize Cart
  cart <- reactiveValues(
    items = data.frame(
      product_id = integer(), 
      Product = character(), 
      Price = numeric(), 
      Quantity = numeric(), 
      Product_Total = numeric(), 
      stringsAsFactors = FALSE
    )
  )
  
  #_Create Shop UI----
  output$uiShop <- renderUI({
    headShop <- div(
      style = 'padding:30px 10% 0 10%;',
      column(
        width = 12,
        align = 'center',
        h1("Fan Shop", style = "font-weight: bold;")
      )
    )
    #_Create Product Cards----
    productCards <- lapply(product$product_id, function(pid) {
      row <- product[product$product_id == pid, ]
      create_product_card(row)
    })
    
    # Combine UI elements
    combinedUI <- tagList(headShop, do.call(tagList, productCards))
    return(combinedUI)
  })
  
  #_Function to create a product card----
  create_product_card <- function(product_row) {
    column(
      width = 6,
      div(
        class = "card equal-height",
        align = 'center',
        style = "width: 24rem; height: 400px; background-color: rgba(255, 255, 255, 0.5); margin: 0 auto;", # Added margin: 0 auto; to help in centering
        img(src = product_row$img, style = "max-height: 200px;"),
        div(
          style = "overflow: hidden;",  # This will hide overflow content
          h4(product_row$name, style="white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"), # These styles will ensure longer names get truncated with ...
          p(paste("$", product_row$price)),
          numericInput(
            inputId = paste0("quantity_", product_row$product_id),
            label = "Select Quantity",
            value = 1,
            min = 1,
            max = 5
          ),
          actionButton(
            inputId = paste0("addToCart_", product_row$product_id),
            label = "Add to Cart"
          )
        )
      )
    )
  }
  
  # Display Cart and Total Price
  output$cartTable <- renderTable({ cart$items })
  output$totalPrice <- renderText({ paste("$", sum(cart$items$Product_Total)) })
  
  #_add products to cart----
  add_to_cart <- function(product_id, quantity) {
    product_row <- product[product$product_id == product_id, ]
    
    if (product_id %in% cart$items$product_id) {
      index <- which(cart$items$product_id == product_id)
      cart$items$Quantity[index] <- cart$items$Quantity[index] + quantity
      cart$items$Product_Total[index] <- cart$items$Price[index] * cart$items$Quantity[index]
    } else {
      item <- data.frame(
        product_id = product_id, 
        Product = product_row$name, 
        Price = product_row$price, 
        Quantity = quantity, 
        Product_Total = product_row$price * quantity
      )
      cart$items <- rbind(cart$items, item)
    }
  }
  
  #_Observers for adding to cart, clearing, and purchasing----
  observe_add_to_cart <- function(pid) {
    observeEvent(input[[paste0("addToCart_", pid)]], {
      quantity <- as.numeric(input[[paste0("quantity_", pid)]])
      add_to_cart(pid, quantity)
    })
  }
  
  lapply(product$product_id, observe_add_to_cart)
  
  observeEvent(input$clearCartButton, {
    cart$items <- data.frame(
      product_id = integer(), 
      Product = character(), 
      Price = numeric(), 
      Quantity = numeric(), 
      Product_Total = numeric()
    )
  })
  
  observeEvent(input$purchaseButton, {
    sendSweetAlert(title = "Purchase Successful!", text = 'Your products will ship in 2-3 days!', type = "success")
  })
  
  
  # >>>>>>>>>>>>>>>>>>----
  # uiFan feedback(5.5)----
  output$uiFanFeed <- renderUI(
    {
      div(
        style = 'padding:60px 10% 0 10%;',
        align = 'left',
        h1(
          style = 'color:navyblue; text-shadow: 3px 3px 5px rgba(250,250,250,0.5);',
          'Fan Feedback'
        ),
        h3('We value your feedback!'),
        h3(
          paste0(
            'Please share your thoughts and suggestions ',
            'with us! Your feedback helps us improve ',
            'the fan experience.'
          )
        ),
        textAreaInput(
          inputId = 'feedbk',
          label = 'Type your feedback here',
          width = '100%',
          height = '100px'
        ),
        hr(),
        h3('Lost & Found'),
        h4(
          paste0(
            'If you have lost an item at an Aviators ',
            'home game, please contact '
          ),
          tags$span(
            tags$a(
              href = 'mailto:aviatorsservice@aviators.com',
              'aviatorsservice@aviators.com'
            ),
            ' or call (999) 999-9999.'
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>----
  # uicommun(6)----
  output$image_carousel <- renderSlickR({
      slickR(image_list, slideId = "slick", width = "600px", height = "400px")
  })
  
  output$uiCommun <- renderUI({
      div(
        h1('Together We Thrive: Vancouver\'s Community Connection'),
        style = 'padding:5vh 15% 35vh 15%;',
        align = 'center',
        fluidRow(
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Stanley Park Environmental Art project'),
              tags$br(),
              h5(style = 'text-align:left;', 'On December 15, 2006, after two short hours of gale-force winds, a storm devastated Stanley Park. Out of the devastation arose opportunities to renew, restore, and respond creatively.'),
              h5(style = 'text-align:left;', 'The Stanley Park Environmental Art Project honours the park and its significance to our city, and on a greater level, comments on sustainability and climate change'),
              tags$a(href = "https://vancouver.ca/parks-recreation-culture/stanley-park-environmental-art-project.aspx", 
                     "Visit City of Vancouver Website for more details", 
                     target = "_blank"),
              tags$br(),
              tags$a(href = "https://vancouver.ca/files/cov/stanley-park-environmental-art-project-self-guided-tour.pdf", 
                     "Take the self-guided Stanley Park tour", 
                     target = "_blank")
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Childcare in the Paint: Basketball\'s Role in Vancouver Community Care'),
              br(),
              h5('Workshop: "From Court to Classroom: Building Skills Beyond the Game"'),
              h5('Discover how basketball mirrors life\'s essential lessons. In this workshop, kids enjoy engaging basketball drills while parents learn to apply game strategies to everyday parenting. Embrace the synergy of teamwork, communication, and dedication both on the court and at home. Join us for a unique fusion of basketball and parenting insights!')
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Vancouver Ocean Embrace Day'),
              br(),
              h5('Submerge yourself in Vancouver\'s marine heritage with the "Vancouver Ocean Embrace Day". An ode to our city\'s intrinsic connection with the ocean, this event celebrates the beauty, significance, and need to protect our coastal treasures'),
              h5('Activity Includes:'),
              h6("Beach Clean-Up"),
              h6("Oceanic Art Workshop"),
              h6("Kayaking & Paddleboarding"),
              h6('Time: 08/03/2023 10:00 - 4:00')
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Urban Garden Day'),
              br(),
              h5('Celebrate the green heart of Vancouver with "Urban Garden Day". This community initiative encourages city dwellers to understand, appreciate, and partake in urban agriculture, transforming concrete landscapes into thriving green spaces.'),
              h5('Accompanied by live folk music, organic food stalls, and DIY craft corners, "Vancouver Urban Garden Day" aims to sow the seeds of community spirit and eco-conscious living. Cultivate joy, health, and community, one garden at a time!'),
              br(),
              h6('Time: 09/16/2023 the whole day')
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Climate action through buildings'),
              br(),
              h5(style = 'text-align:left;', 'Burning natural gas, a fossil fuel, in buildings (for space and water heating) accounts for 55% of the carbon pollution generated in Vancouver. Construction creates additional emissions locally and globally through the production and transportation of building materials.'),
              h5('2030 targets:'),
              h6('Carbon pollution from buildings will be half what it was in 2007'),
              h6('There will be 40% less embodied emissions from new buildings and construction projects compared to 2018'),
              tags$a(href = "https://vancouver.ca/green-vancouver/buildings.aspx",
                     "Visit City of Vancouver Website for more details", 
                     target = "_blank"),
            )
          ),
          column(
            width = 6,
            wellPanel(
              style = 'min-height:300px; background-color: rgba(255, 255, 255, 0.8);', 
              h4('Your generous contributions make a tremendous impact! '),
              br(),
              h5('Donations of high-need items such as non-perishable and fresh food, outdoor supplies, personal hygiene items, household items, and seasonal clothing empower our clients to grow and thrive. '),
              h6('We express immense gratitude to our community members, local businesses, and all who have stood by this initiative. With every thread donated, we weave a stronger, warmer, and more hopeful Vancouver. Join us, and let\'s clothe our community with love and care.'),
              tags$a(href = "https://sharevancouver.org/donate-now/donate-resources/", 
                     "Visit 'Share Vancouver' Website for more details", 
                     target = "_blank"),
            )
          )
        )
      )
    })
  
  
  # >>>>>>>>>>>>>>>>>>----
  # uiPartners(7)----
  partner_selected <- reactiveVal()
  
  output$uiPartners <- renderUI({
    div(
      style = 'padding:15vh 15% 0 15%;',
      align = 'center',
      lapply(
        0:2, 
        function(i) {
          fluidRow(
            lapply(
              1:3,
              function(j) {
                p <- i * 3 + j
                column(
                  width = 4,
                  div(
                    style = 'display: flex; justify-content: center; align-items: center; height: 100%;',
                    style = paste0(
                      'padding:10px 20px 10px 20px; ',
                      'margin:10px 0 10px 0; ',
                      'border:light orange 0; border-radius:10px; ',
                      'background-color:white; ',
                      'box-shadow: 0 4px 8px 0 rgba(0,0,0,0.5);'
                    ),
                    tags$button(
                      id = paste0('partner', p),
                      class = 'btn action-button',
                      style = 'background-color:rgba(0,0,0,0); padding:0; border-width:0; display: block; width: 150px; height: 150px;',
                      img(
                        src = paste0('partners/', p, '.jpg'),
                        style = 'width: 100%; height: 100%; object-fit: contain; display: block; border: none; padding: 0; margin: 0;' # Use object-fit: contain; here
                      )
                    )
                  )
                )
              }
            )
          )
  }),
  br(),
  uiOutput('uiPartner')
    )
  })
  
  for (i in 1:9) {
    local({
      my_i <- i
      observeEvent(input[[paste0('partner', my_i)]], {
        partner_selected(my_i)
      })
    })
  }
  
  observe({
    if (!is.null(partner_selected()) && partner_selected() > 0) {
      pp <- dbGetQuery(
        conn = con,
        sprintf('SELECT partner_name, partner_industry FROM partner WHERE partner_id = %d', partner_selected())
      )
      pp2 <- dbGetQuery(
        conn = con,
        sprintf('SELECT partner_id, event_id, event_name, event_desc FROM events JOIN partner_events USING (event_id) WHERE partner_id = %d', partner_selected())
      )
      
      # Create an organized view for the modal
      modal_content <- tagList(
        div(
          style = "text-align: center; padding-top: 0px;",
          tags$h2(tags$strong(pp$partner_name)), 
          tags$h4(paste0(pp$partner_industry, " Industry"))
        ),
        hr(),
        div(
          style = "text-align: center; padding-top: 0px;",
          h3(tags$strong('Upcoming Events:')),
          lapply(1:nrow(pp2), function(i) {
            list(
              tags$h4(pp2$event_name[i]),
              tags$p(pp2$event_desc[i])
            )
          })
        )
      )
      
      # Show the modal
      showModal(modalDialog(
        modal_content,
        footer = tagList(
          actionButton("close", "Close")
        ),
        easyClose = TRUE,
        class = "centered-title"
      ))
    }
  })
  
  observeEvent(input$close, {
    removeModal()
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiBasketball history(8.1)----
  output$uiBask <- renderUI({
    div(
      style = 'padding:50px 5% 0 5%;',
      align = 'center',
      h2(
        style = 'color:white;text-shadow: 2px 2px black;',
        tags$strong('Basketball History in Vancouver')
      ),
      hr(),
      wellPanel(
        style = 'padding-top:5px;text-align:left;background-color: rgba(255, 255, 255, 0.8);',
        h3('Former NBA team in Vancouver: Vancouver Grizzlies(1995-2001)'),
        h4('Reasons to leave Vancouver:'),
        tags$ul(
          tags$li(tags$strong('Financial Struggles:'),'The team consistently lost money during its tenure in Vancouver. The weak Canadian dollar at that time meant higher operational costs compared to U.S.-based teams.'),
          tags$li(tags$strong('Poor On-Court Performance'),'The Grizzlies struggled in terms of on-court performance, which led to decreased fan interest and attendance.'),
          tags$li(tags$strong('Ownership Issues:'), 'Michael Heisley bought the team in 2000 and soon after began exploring relocation options, feeling he could find a more lucrative market in the U.S.')
        ),
        p('By 2001, these issues culminated in the Grizzlies\' move to Memphis, Tennessee, where they were rebranded as the Memphis Grizzlies. The move left a void in Vancouver, a city that had hoped to solidify its position in the NBA landscape.')
      ),
      br(),
      wellPanel(
        style = 'padding-top:5px;text-align:left;background-color: rgba(255, 255, 255, 0.8);',
        h3('Vancouver Bandits'),
        h4('Founded: 2018'),
        h5('The Vancouver Bandits were a team in the National Basketball League of Canada (NBL Canada), one of the top professional basketball leagues in Canada.'),
        tags$a(href = "https://www.thebandits.ca/", 
               "Visit Vancouver Bandits Website", 
               target = "_blank")
        # br(),
        # tags$img(src = "vanB.png", height = "100px", width = "100px")
      ),
      br(),
      wellPanel(
        style = 'padding-top:5px;text-align:left;background-color: rgba(255, 255, 255, 0.8);',
        h3('University Basketball'),
        h5('The Vancouver region boasts some outstanding university basketball teams, including those from the University of British Columbia and Simon Fraser University. These teams have achieved commendable results in the U Sports league, Canada\'s university basketball league.'),
        tags$ul(
          tags$li(tags$strong('UBC Thunderbirds: '), 'The Thunderbirds are the athletic teams that represent the University of British Columbia. Both the men\'s and women\'s basketball teams of UBC have historically been strong contenders in the U Sports Canada West conference.'),
          tags$li(tags$strong('Simon Fraser University (SFU): '),'Located in Burnaby, a suburb of Vancouver, SFU is unique as it\'s the only Canadian university that competes in the NCAA (National Collegiate Athletic Association) in the United States.')
        ),
        tags$img(src = "UBC_Thunderbirds_Logo.png", height = "80px", width = "60px"),
        tags$img(src = "SFU.png", height = "80px", width = "60px", style= "margin-left: 20px;"),
      ),
      wellPanel(
        style = 'padding-top:5px;text-align:left;background-color: rgba(255, 255, 255, 0.8);',
        h3('Basketball Events and Community Organizations '),
        h5('Vancouver has hosted several basketball events, including NBA preseason games and FIBA (International Basketball Federation) matches. Additionally, community organizations and basketball clubs offer opportunities for local residents to engage in the sport.'),
        h6('Some Famous Clubs in Vancouver:'),
        tags$ul(
          tags$li(tags$a(href = "https://www.raincitybasketball.ca/", "RainCity Basketball Vancouver"))
        ),
        tags$ul(
          tags$li(tags$a(href = "https://www.dynamitebasketball.com/", "Dynamite Basketball Club"))
        ),
        tags$ul(
          tags$li(tags$a(href = "https://vancouvervolcanoes.com/", "Vancouver Volcanoes"))
        )
      )
    )
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiAbout Vancouver(8.2)----
  
  #_Plots used in about Vancouver page----
  # Vancouver Gender Data
  gender_data <- data.frame(
    Gender = c("Women", "Men"),
    Count = c(337675, 324570),
    Percentage = c(51, 49)
  )
  
  # pie chart
  gender_plot <- ggplot(gender_data, aes(x = "", y = Count, fill = Gender)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(Percentage, "%", " (", Count , ")")), 
              position = position_stack(vjust = 0.5),
              size = 4) +
    labs(title = "Gender of Residents of the City of Vancouver\n Dara Source:The 2021 Census reports",
         fill = "Gender",
         x = NULL,
         y = NULL) +
    theme_void() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(), 
          legend.margin = margin(t = 0, unit = "cm"), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          axis.text = element_blank(), 
          panel.grid = element_blank())
  
  custom_colors <- c("#FF9999", "#66B3FF")
  gender_plot <- gender_plot + scale_fill_manual(values = custom_colors)
  
  # Persons by Income Level (2017-2021) Data
  income_data <- data.frame(
    "Income_Level" = c("Under $5,000", "$5,000+", "$10,000+", "$15,000+",
                       "$20,000+", "$25,000+", "$35,000+",
                       "$50,000+", "$75,000+", "$100,000+",
                       "$150,000+", "$200,000+", "$250,000+"),
    "2017" = c(166510, 1794320, 1675960, 1509830, 1336290, 1196160, 977050, 686560, 367400, 196530, 74120, 37470, 22890),
    "2018" = c(165830, 1850760, 1732790, 1575060, 1399370, 1252560, 1025880, 725060, 390030, 210400, 80270, 41310, 25430),
    "2019" = c(158880, 1921440, 1797680, 1652470, 1463260, 1309570, 1079130, 773110, 423060, 230760, 88370, 45420, 27840),
    "2020" = c(104230, 1993340, 1908410, 1808960, 1666010, 1481030, 1181640, 837650, 459610, 252320, 95680, 48810, 29430),
    "2021" = c(135820, 1990760, 1902350, 1798130, 1645560, 1474250, 1218260, 892480, 510030, 290480, 114720, 59230, 35740)
  )
  
  income_data_long <- tidyr::pivot_longer(income_data, cols = -"Income_Level", names_to = "Year", values_to = "Count")
  
  income_plot <- ggplot(income_data_long, aes(x = Year, y = Count, fill = `Income_Level`)) +
    geom_bar(stat = "identity") +
    labs(title = "Persons by Income Level (2017-2021)",
         x = "Year",
         y = "Persons",
         fill = "Income_Level") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.title.position = "plot")
  
  # Population Distribution Data
  pop_data <- data.frame(
    Characteristic = c(
      "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years",
      "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years",
      "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years",
      "60 to 64 years", "65 years+"
    ),
    Men = c(12170, 12140, 12280, 13645, 21135, 32785, 34580, 29065, 22380, 20055, 21665, 22060, 19060, 51555),
    Women = c(11670, 10930, 11380, 12670, 22155, 34375, 34640, 28270, 22695, 21990, 23175, 22305, 20265, 61160)
  )
  
  pop_data_long <- tidyr::pivot_longer(pop_data, cols = c(Men, Women), names_to = "Gender", values_to = "Population")
  
  # bar chart
  pop_plot <- ggplot(pop_data_long, aes(x = Characteristic, y = Population, fill = Gender)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Population Distribution by Age and Gender", y = "Population Count") +
    scale_fill_manual(values = c("Men" = "blue", "Women" = "pink")) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.title.position = "plot")
  
  # GDP Data
  gdp_data <- data.frame(
    Geography = c("Vancouver, British Columbia (map)"),
    `X2015` = c(131406),
    `X2016` = c(138344),
    `X2017` = c(145756),
    `X2018` = c(154280),
    `X2019` = c(163475),
    `X2020` = c(143000),
    `X2021` = c(149578),
    `X2022` = c(153766),
    `X2023PREDICT` = c(158000)
  )
  gdp_data_long <- pivot_longer(gdp_data, cols = c(`X2015`, `X2016`, `X2017`, `X2018`, `X2019`,`X2020`,`X2021`, `X2022`, `X2023PREDICT`), names_to = "Year", values_to = "Dollars")
  
  # Line Chart
  gdp_plot <- ggplot(gdp_data_long, aes(x = Year, y = Dollars, color = Geography, group = Geography)) +
    geom_line() +
    geom_point() +
    labs(title = "Gross domestic product (GDP) at basic prices", y = "Dollars") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.title.position = "plot")
  
  #_renderPlot----
  output$plot_pie_chart <- renderPlot({
    gender_plot
  })
  
  output$plot_income_level <- renderPlot({
    income_plot
  })
  
  output$plot_population_dist <- renderPlot({
    pop_plot
  })
  
  output$plot_gdp <- renderPlot({
    gdp_plot
  })
  
  #_uiAbout Van----
  output$uiAboutVan <- renderUI({
    div(
      style = 'padding:50px 5% 0 5%;',
      align = 'center',
      h2(
        style = 'color:white;text-shadow: 2px 2px black;',
        tags$strong('Demographic and Economic Insights of Vancouver')
      ),
      hr(),
      div(style = 'width: 100%; display: flex; flex-wrap: wrap; justify-content: space-between;',
          div(
            style = 'width: 48%; margin-bottom: 2%; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2); padding: 2%; background-color: rgba(128, 128, 128, 0.5);',
            plotOutput("plot_gdp"),
            tags$p(style = "color: white; font-size: 18px;","This plot shows Vancouver's GDP growth from 2015 to 2023 (predicted). Despite a dip in 2020, Vancouver has consistently shown economic resilience and growth. The upward trajectory in GDP underscores Vancouver's robust economy, making it a promising location for introducing our NBA team 'Vancouver Aviators' to Vancouver, as a thriving economy can support and sustain a major sports franchise.")
          ),
          div(
            style = 'width: 48%; margin-bottom: 2%; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2); padding: 2%; background-color: rgba(128, 128, 128, 0.5);',
            plotOutput("plot_income_level"),
            tags$p(style = "color: white; font-size: 18px;","The income_data from 2017 to 2021 reveals a growing middle and upper-middle-class in Vancouver, crucial for NBA game attendance."),
            tags$p(style = "color: white; font-size: 18px;","Despite an economic shift in 2020, there's an upward trend in higher income brackets. From the plot, we can see potentials for both premium ticket sales and broader audience engagement for our future Vancouver Aviators NBA games.")
          ),
          div(
            style = 'width: 48%; margin-bottom: 2%; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2); padding: 2%; background-color: rgba(128, 128, 128, 0.5);',
            plotOutput("plot_population_dist"),
            tags$p(style = "color: white; font-size: 18px;","Notably, the 25-34 age range has the highest population, suggesting a young, active demographic. While the population of women slightly surpasses that of men, especially in the 65 years+ bracket, both genders show a similar trend across age groups. This data is vital for tailored marketing or community engagement efforts in Vancouver")
          ),
          div(
            style = 'width: 48%; margin-bottom: 2%; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2); padding: 2%; background-color: rgba(128, 128, 128, 0.5);',
            plotOutput("plot_pie_chart"),
            tags$p(style = "color: white; font-size: 18px;","The near-equal gender distribution means that there's an opportunity to create a diverse and inclusive fan culture from the outset, which could set the Vancouver NBA team apart and ensure its success in the long run.")
          )
      )
    )
  })
  
  # >>>>>>>>>>>>>>>>>>----
  # uiAbout Us(9)----
  mems <- c('Jack Zhang', 'Yixuan Li', 'Keni Ding', 'Vivian Wen', 'Xinyu Wang', 'Bing Lu', 'Yixuan Jiang', 'Yiting Feng')
  texts <- c(
    'Hey folks! I was born in Shenyang, China. I graduated from Purdue University with a BS in Accounting and University of San Diego with JD in Law, and am currently pursuing a degree in Applied Analytics at Columbia University. I have been a big NBA fan for 20 years and have practically witnessed the amazing trends in basketball for the last 2 decades.',
    'Hello, I am Yixuan Li. I was born in Beijing, China. I graduated from the University of Connecticut with Econ major and Math minor. Now I am in Columbia University, a student in the field of Applied Analytics for my Master Degree.',
    'Born in Wuhan, China. As a Marketing Specialist for the Vancouver NBA team, I am currently pursuing my degree in Columbia University, I am driven to combine my passion for sports with my expertise in Applied Analytics. Let us create unforgettable experiences and build a strong fan base for our team!',
    'Born in Sichuan, China. UW Economics undergrad, Columbia University MS in Applied Economics. Love NBA! Passionate about basketball analytics and management. Let us connect and share our NBA passion!',
    'I was born in Dalian, China. Graduated from the University of Toronto with a double major in mathematics and statistics, studying a degree in Applied Analytics at Columbia University. A newcomer to the exciting world of NBA, hoping to delve into the fascinating world of basketball together.',
    'I was born in Anhui, China, and I graduated from University of California, Irvine for a bachelor degree with a major in Business Economics. Now I am pursuing a master degree of Applied Analytics in Columbia University. I am passionate about athletics, especially basketball so I hope we can build a professional team in all aspects together.',
    'I am Yixuan Jiang, hailing all the way from Zhejiang, China! So, back in the day, I was busting my brain studying STAT at the cool University of Maryland for my undergrad degree. And guess what? I leveled up my education game and went for a graduate degree in Applied Analytics at Columbia University!',
    'Ayo! I am Yiting Feng, a Master student in Applied Analytics at Columbia University with a profound connection to Vancouver, Canada, where I completed my Bachelor Degree. Driven by deep love for VanCity and a great passion for basketball, my team is the driving force behind the ambitious NBA project aiming at bringing a professional basketball team back to the vibrant streets of Vancouver.'
  )
  
  output$uiOurTeam <- renderUI({
    div(
      style = 'padding:50px 5% 0 5%;',
      align = 'center',
      h3(
        style = 'color:white;',
        'Our Team'
      ),
      hr(),
      fluidRow(
        lapply(
          1:4, 
          function(i) {
            column(
              width = 2,
              offset = ifelse(i == 1, 2, 0),
              tags$div(
                style = 'padding:5px; background-color:rgba(0, 0, 0, 0.5); box-shadow: 2px 2px 4px #888888; text-align:center;min-width: 210px;',
                tags$button(
                  id = paste0('memberBtn', i),
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; border-width:0;',
                  img(
                    src = paste0('members/member', i, '.jpg'),
                    width = '80px',
                    height = '100px'
                  )
                ),
                h4(
                  style = 'color:white; margin-top: 5px;',
                  mems[i]
                ),
                h6(
                  style = 'color:white; margin-top: 5px;',
                  staff$staff_role[i]
                ),
                tags$a(
                  href = "javascript:void(0);",
                  onclick = sprintf("window.open('%s', '_blank');", staff$linkedin[i]),
                  img(src = "linkedin.png", width = "35px", height = "15px")
                )
              )
            )
          }
        )
      ),
      br(),
      fluidRow(
        lapply(
          5:8, 
          function(i) {
            column(
              width = 2,
              offset = ifelse(i == 5, 2, 0),
              tags$div(
                style = 'padding:5px; background-color:rgba(0, 0, 0, 0.5); box-shadow: 2px 2px 4px #888888; text-align:center;min-width: 210px;',
                tags$button(
                  id = paste0('memberBtn', i),
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; border-width:0;',
                  img(
                    src = paste0('members/member', i, '.jpg'),
                    width = '80px',
                    height = '100px'
                  )
                ),
                h4(
                  style = 'color:white; margin-top: 5px;',
                  mems[i]
                ),
                h6(
                  style = 'color:white; margin-top: 5px;',
                  staff$staff_role[i]
                ),
                tags$a(
                  href = "javascript:void(0);",
                  onclick = sprintf("window.open('%s', '_blank');", staff$linkedin[i]),
                  img(src = "linkedin.png", width = "40px", height = "20px")
                )
              )
            )
          }
        )
      ),
      fluidRow(
        column(
          width = 8,
          offset = 2,
          tags$textarea(
            id = 'memberText',
            style = 'color:black; 
             background-color: rgba(255, 255, 255, 0.5); 
             text-align:center; 
             border-width: 0; 
             font-weight: bold; 
             font-size: 15px;
             width: 80%; 
             height: 100px;',
            placeholder = "Click a member's photo to explore further."
          )
        )
      )
    )
  })
  
  #_Update text when a member button is clicked----
  observe({
    selected_member <- mems[m$i]
    selected_text <- texts[m$i]
    updateTextAreaInput(session, "memberText", value = selected_text)
  })

  # _events click member----
  observeEvent(input$memberBtn1, {m$i <- 1})
  observeEvent(input$memberBtn2, {m$i <- 2})
  observeEvent(input$memberBtn3, {m$i <- 3})
  observeEvent(input$memberBtn4, {m$i <- 4})
  observeEvent(input$memberBtn5, {m$i <- 5})
  observeEvent(input$memberBtn6, {m$i <- 6})
  observeEvent(input$memberBtn7, {m$i <- 7})
  observeEvent(input$memberBtn8, {m$i <- 8})
  
}
