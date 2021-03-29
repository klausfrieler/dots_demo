library(psyquest)
library(psychTestR)

library(shiny)
source("./utils.R")

num_items <- c(BAT = 3,EDT = 3, JAJ = 3, MDT = 3, MIQ = 3, MPT = 3, RAT = 3, PIT = 3, BDS = 3) 
#num_items <- c(BAT = 1,EDT = 1, JAJ = 1, MDT = 1, MIQ = 1, MPT = 1, RAT = 1) 
take_training <- T

all_tests <- c(
  "DEG", "GMS", "BAT", "MDT", "MPT", "CCM", "DAC", "MHE", "PAC", "SCA", "SCS", "SDQ", "SEM", "TOI", "TOM", "SMP", "TPI", 
  "EDT", "JAJ", "MIQ", "RAT", "GRT", "HOP", "BDS"
)
test_names <- list("HD0" = "Musikalische Hörtests",
                   "BAT" = c("name" = "Beatwahrnehmungs-Test", 
                             "git_repo" = "https://github.com/pmcharrison/cabat", 
                             "ref_paper" ="https://www.nature.com/articles/s41598-018-30318-8"),
                   "MDT" = c("name" = "Melodieunterscheidungs-Tests",
                             "git_repo" = "https://github.com/pmcharrison/mdt",
                             "ref_paper" = "https://www.nature.com/articles/s41598-017-03586-z"),
                   "MPT" = c("name" = "Verstimmungswahrnehmungs-Test",
                             "git_repo" = "https://github.com/pmcharrison/mpt",
                             "ref_paper" = "https://link.springer.com/article/10.3758%2Fs13428-019-01225-1"), 
                   "RAT" = c("name" = "Rhythmusfähigkeits-Test",
                             "git_repo" = "https://github.com/klausfrieler/RAT",
                             "ref_paper" = ""),
                   "PIT" = c("name" =" Tonvorstellungs-Test",
                             "git_repo" = "https://github.com/pmcharrison/piat",
                             "ref_paper" = "https://link.springer.com/article/10.1007/s00426-020-01322-3"),
                   "EDT" = c("name" = "Emotionenunterscheidungs-Test",
                             "git_repo" = "https://github.com/klausfrieler/EDT",
                             "ref_paper" = "https://www.frontiersin.org/articles/10.3389/fpsyg.2019.01955/full"),
                   
                   "HD1"  = "Nicht-musikalische Leistungstests",
                   #"MIQ" = "Cognitive Puzzles Test",
                   "JAJ" = c("name" = "Jack & Jill Arbeitsgedächtnis-Test",
                             "git_repo" = "https://github.com/klausfrieler/JAJ",
                             "ref_paper" = ""),
                   "BDS" = c("name" = "Backward Digit Span Arbeitsgedächtnis-Test",
                             "git_repo" = "https://github.com/klausfrieler/BDS",
                             "ref_paper" = ""),
                   "HD2" = "Selbstauskunftsfragebögen zu musikalischen und anderen Aktivitäten",
                   "GMS" = c("name" = "Goldsmiths Musical Sophistication Index",
                             "git_repo" = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0089642"),
                   "CCM" = c("name" = "Fragebogen zur aktuellen musikalische Altivititäten ",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "MHE" = c("name" = "Fragebogen zum häuslischen musikalischen Umgebung ",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "DAC" = c("name" = "Fragebogen zu Theateraktivittäten",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "PAC" = c("name" = "Fragebogen zu sportlichen Aktivitäten",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "HD3" = "Selbstauskunftsfragebogen zu psychosozialen Faktoren",
                   "DEG" = c("name" = "Basisdemographische Angaben",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SES" = c("name" = "Selbsauskunftsinventar zum sozio-ökonomischen Status",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   #"SCA" = "Academic Self-Concept Questionnaire",
                   #"SCS" = "Social Self-Concept Questionnaire",
                   "TOM" = c("name" = "Fragen zur Theorie der  Musikalität",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "TOI" = c("name" = "Fragebogen zur Theorie of Intelligenz",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SDQ" = c("name" = "Fragebiogen zu Stärken und Schwächen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SEM" = c("name" = "Fragebogen zu Schulengagement",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "HOP" = c("name" = "Hoffnungsskala für Kinder",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "GRT" = c("name" = "Durchhaltevermögensskala für Kinder",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "SMP" = c("name" = "Kurzer Test zu musikalischen Präferenzen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "MUS" = c("name" = "Kurzer klingender Test zu musikalischen Präferenzen",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "TPI" = c("name" = "10-Item Persönlichkeitsinventar",
                             "git_repo"  = "https://github.com/fmhoeger/psyquest",
                             "ref_paper" = ""), 
                   "NA" = "")

include_test <- function(test_id ){
  function(state, session, ...){
    #browser()
    tests <- get_test_id_from_url(state, session)
    if(is.null(tests) || nchar(tests) == 0){
      tests <- all_tests
      tests <- "NA"
    }
    messagef("Checking %s, found tests %s", test_id, paste(tests, collapse = ","))
    test_id %in% tests  
  }
}


selection_page <- function(){
  psychTestR::code_block(function(session, ...){
    base_url <- sprintf("%s//%s:%s%s", 
                        session$clientData$url_protocol,
                        session$clientData$url_hostname,
                        session$clientData$url_port,
                        session$clientData$url_pathname)
    
    body_text <- 
      map(names(test_names), function(tn){
        browser()
        if(tn == "HD"){
          shiny::h3(test_names[[tn]]) 
        }
        else{
          if(length(names(test_names[[tn]])) > 0){
            
            href <- sprintf("%s?test=%s", base_url, tn)
            shiny::p(
              shiny::a(href = href, target = "_blank", test_names[[tn]]["name"]), 
              shiny::span(
                shiny::a(href = test_names[[tn]]["git_repo"], target = "_blank", "Github"), 
                shiny::a(href = test_names[[tn]]["ref_paper"], target = "_blank", "Paper"), 
                
              )
            )
            
          }
          else{
            href <- sprintf("%s?test=%s", base_url, tn)
            shiny::p(
              shiny::a(href = href, target = "_blank", test_names[[tn]])
            )
          }
        }
      })
    print(body_text)
    psychTestR::final_page(
      body = shiny::p(body_text))
    
  })
}

get_test_name <- function(test_id){
  tmp <- test_names[[test_id]]
  if("names" %in% names(tmp)){
    return(tmp[["name"]])
  }
}
get_test_prop <- function(test_id, prop){
  tmp <- test_names[[test_id]]
  if(prop %in% names(tmp)){
    return(tmp[[prop]])
  }
  tmp
}
static_selection_page <-function(){
  if(local_debug){
    base_url <- "http://127.0.0.1:3973/"
    
  }
  else{
    base_url <- "https://testing.musikpsychologie.de/dots_demo/"
    
  }
  body_text <- 
    map(names(test_names), function(tn){
      #browser()
      if(substr(tn, 1, 2) == "HD"){
        shiny::p(
          shiny::tags$b(test_names[tn], style = "text-align:left;"), style = "text-align:left; margin-left:20%;margin-top:30px;"
        )
      }
      else{
        href <- sprintf("%s?test=%s", base_url, tn)
        if("name" %in% names(test_names[[tn]])){
          git_repo <- get_test_prop(tn, "git_repo")
          ref_paper <- get_test_prop(tn, "ref_paper")
          shiny::p(
            shiny::a(href = href, target = "_blank", get_test_prop(tn, "name")), 
            
            shiny::span(
              if(nchar(git_repo) > 0) shiny::a(href = git_repo, 
                                               target = "_blank", 
                                               "[GitHub]",
                                               style = "color:#f47920;text-decoration:none"), 
              if(nchar(ref_paper) > 0) shiny::a(href = ref_paper, 
                                                target = "_blank", 
                                                "[Quelle]", 
                                                style = "color:#f47920;text-decoration:none"),
              style = "font-size:10pt;margin-left:5pt"
            ),
            style = "text-align:left; margin-left:20%")
          
        }
        else{
          shiny::p(
            shiny::a(href = href, target = "_blank", get_test_prop(tn, "name")), 
            style = "text-align:left; margin-left:20%;")
          
        }
      }
    })
  
  psychTestR::final_page(
    body = shiny::div(shiny::h2("Willkommen zu DOTS Demo Seite", style = "text-align:left; margin-left:20%"), 
                      #shiny::p("Please choose a test.", style = "text-align:left; margin-left:20%"), 
                      shiny::includeHTML("intro_de.html"), 
                      shiny::p(body_text, style = "text-align:left; margin-left:00%")))
}

welcome_finished_page <- function(type, test_id){
  if("name" %in% names(test_names[[test_id]])){
    test_name <- test_names[[test_id]]["name"]
  }
  else{
    test_name <- test_names[[test_id]]
  }
  if(type == "welcome"){
    body_text <- shiny::h3(sprintf("Willkomen zum %s", test_name))
  }
  else{
    body_text <- sprintf("Du hast den  %s beendet.", test_name)
  }
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = body_text,
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = JAJ::JAJ_dict)
}

as_timeline <- function(mixed_list){
  #do.call(psychTestR::join, mixed_list)  
  mixed_list
}


wrap_quest_full_demo <- function(quest, test_name = "NA"){
  if(test_name == "NA"){
    browser()
  }
  psychTestR::join(
    welcome_finished_page("welcome", test_name), 
    quest) %>%  
    as_timeline()
}
GMS_feedback_barplot <- function(results,  width = 600, height = 450){
  
  GMS_scales <- c("Active Engagement",  "Singing Abilities", "Perceptual Abilities", 
                  "Musical Training", "Emotions")
  results <- as.list(results)
  #results <- results[[length(results)]]
  if(!("GMS" %in% names(results))){
    return()
  }
  
  final_scores <- tibble(scale = factor(GMS_scales), scores = unlist(results$GMS[GMS_scales]))
  q <- final_scores %>% ggplot(aes(x = scale, y = scores, fill = scale)) 
  q <- q + geom_bar(stat = "identity") 
  q <- q + geom_text(aes(y = scores + .05, label = round(scores, 1)), hjust = 0)
  q <- q + theme_bw(base_size = 12)
  q <- q + coord_flip()
  q <- q + labs(x = "", y = "Mittelwert")
  q <- q + scale_y_continuous(limits = c(0,7))
  q <- q + theme(legend.title = element_blank(), legend.position = "none")
  return(q)
  
}

feedback_graph_normal_curve <- function(score, test_name = "Gold-MSI",
                                        x_min = 32, x_max = 126, 
                                        x_mean = 82, x_sd = 10, 
                                        width = 600, height = 450){
  fakeIQ <- max(min(score, x_max), x_min)
  messagef("%s score: %f, fakeIQ: %f", test_name, score, fakeIQ)
  q <- ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) + 
    ggplot2::stat_function(fun = dnorm, args = list(mean = x_mean, 
                                                    sd = x_sd)) + 
    ggplot2::stat_function(fun = dnorm,  args = list(mean = x_mean, sd = x_sd), 
                           xlim = c(x_min,  fakeIQ), 
                           fill = "lightblue4", 
                           geom = "area")
  q <- q + ggplot2::theme_bw()
  x_axis_lab <- sprintf("%s Score", test_name)
  title <- "Your total score"
  main_title <- sprintf("%s: %.0f", title, round(fakeIQ, digits = 0))
  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  return(q)
}

GMS_norm_plot <- function(results,  width = 600, height = 450){
  results <- as.list(results)
  if(!("GMS" %in% names(results))){
    return()
  }
  feedback_graph_normal_curve(results$GMS$General*18,  width = width, height = height)
}

GMS_full_feedback <- function(){
  psychTestR::reactive_page(function(state, ...){
    results <- psychTestR::get_results(state, complete = T)
    bar_plot <- GMS_feedback_barplot(results,  width = 450, height = 300)
    norm_plot <- GMS_norm_plot(results,  width = 450, height = 300)
    combined_plot <- plotly::subplot(norm_plot, bar_plot, widths = c(.5, .5), margin = .1)
    psychTestR::page(ui = shiny::div(shiny::h4("Das sind deine Ergebnisse"),
                                     shiny::p(combined_plot, 
                                              style = "display:inline-block;float:left;width:100%"), 
                                     shiny::p(psychTestR::trigger_button("next", "Weiter"))))
  })
}  

dots_demo  <- function(title = "DOTS Demo",
                                documentation = "DOTS",
                                admin_password = "dotsdemo",
                                researcher_email = "kf@omniversum.de",
                                languages = "de",
                                dict = psyquest::psyquest_dict,
                                ...) {
  elts <- join(
    psychTestR::conditional(include_test("NA"), psychTestR::new_timeline(static_selection_page(), dict = dict)),
    psychTestR::conditional(include_test("DEG"), wrap_quest_full_demo(psyquest::DEG(), "DEG")),
    psychTestR::conditional(include_test("SES"), wrap_quest_full_demo(psyquest::SES(), "SES")),
    #psychTestR::conditional(include_test("MIQ"),
    #                        psychTestR::join(
    #                          welcome_finished_page("welcome", "MIQ"),
    #                          MIQ::MIQ(num_items = num_items[["MIQ"]], 
    #                                   take_training = take_training, 
    #                                   with_welcome = F,
    #                                   with_finish = F,
    #                                   feedback = MIQ::feedback_with_graph()))),
    psychTestR::conditional(include_test("CCM"), wrap_quest_full_demo(psyquest::CCM(), "CCM")),
    psychTestR::conditional(include_test("MPT"), 
                            psychTestR::join(
                              welcome_finished_page("welcome", "MPT"),
                              mpt::mpt(num_items = num_items[["MPT"]], 
                                       take_training = take_training,
                                       feedback = psychTestRCAT::cat.feedback.graph(test_label = "MPT")),
                              welcome_finished_page("finished", "MPT")
                            )),
    psychTestR::conditional(include_test("PIT"), 
                            psychTestR::join(
                              welcome_finished_page("welcome", "PIT"),
                              piat::piat(num_items = num_items[["PIT"]], 
                                         take_training = take_training,
                                         feedback = psychTestRCAT::cat.feedback.graph(test_label = "PIT")),
                              welcome_finished_page("finished", "PIT")
                            )),
    psychTestR::conditional(include_test("TOI"), wrap_quest_full_demo(psyquest::TOI(), "TOI")),
    psychTestR::conditional(include_test("JAJ"), 
                            psychTestR::join(
                              JAJ::JAJ(num_items = num_items[["JAJ"]], 
                                       take_training = take_training,
                                       feedback = JAJ::JAJ_feedback_with_graph()))),
    psychTestR::conditional(include_test("BDS"), 
                            psychTestR::join(
                              BDS::BDS(num_items = num_items[["BDS"]], 
                                       with_training = take_training,
                                       feedback = BDS::BDS_feedback_with_graph()))),
    psychTestR::conditional(include_test("TOM"), wrap_quest_full_demo(psyquest::TOM(), "TOM")),
    psychTestR::conditional(include_test("BAT"), 
                            psychTestR::join(
                              welcome_finished_page("welcome", "BAT"),
                              cabat::cabat(num_items = num_items[["BAT"]],
                                           take_training = take_training,
                                           feedback = psychTestRCAT::cat.feedback.graph(test_label = "BAT"))
                            )),    
    psychTestR::conditional(include_test("MHE"), wrap_quest_full_demo(psyquest::MHE(), "MHE")),
    psychTestR::conditional(include_test("GMS"), 
                            psychTestR::join(
                              wrap_quest_full_demo(psyquest::GMS(short_version = T), "GMS"),
                              GMS_full_feedback()
                            )),
    psychTestR::conditional(include_test("MDT"), 
                            psychTestR::join(
                              welcome_finished_page("welcome", "MDT"),
                              mdt::mdt(num_items = num_items[["MDT"]], 
                                       take_training = take_training,
                                       feedback = psychTestRCAT::cat.feedback.graph(test_label = "MDT")),
                              welcome_finished_page("finished", "MDT")
                            )),    
    psychTestR::conditional(include_test("DAC"), wrap_quest_full_demo(psyquest::DAC(), "DAC")),
    psychTestR::conditional(include_test("PAC"), wrap_quest_full_demo(psyquest::PAC(), "PAC")),
    psychTestR::conditional(include_test("SDQ"), wrap_quest_full_demo(psyquest::SDQ(), "SDQ")),
    psychTestR::conditional(include_test("RAT"), 
                            psychTestR::join(
                              RAT::RAT(num_items = num_items[["RAT"]], 
                                       take_training = take_training,
                                       feedback = RAT::RAT_feedback_with_graph())
                            )),
    psychTestR::conditional(include_test("SMP"), wrap_quest_full_demo(psyquest::SMP(), "SMP")),
    psychTestR::conditional(include_test("MUS"), wrap_quest_full_demo(psyquest::MUS(), "MUS")),
    psychTestR::conditional(include_test("TPI"), wrap_quest_full_demo(psyquest::TPI(), "TPI")),
    psychTestR::conditional(include_test("SCS"), wrap_quest_full_demo(psyquest::SCS(short_version = T), "SCS")),
    psychTestR::conditional(include_test("SCA"), wrap_quest_full_demo(psyquest::SCA(short_version = T), "SCA")),
    psychTestR::conditional(include_test("HOP"), wrap_quest_full_demo(psyquest::HOP(), "HOP")),
    psychTestR::conditional(include_test("GRT"), wrap_quest_full_demo(psyquest::GRT(), "GRT")),
    psychTestR::conditional(include_test("SEM"), wrap_quest_full_demo(psyquest::SEM(), "SEM")),
    psychTestR::conditional(include_test("EDT"), 
                            psychTestR::join(
                              EDT::EDT(num_items = num_items[["EDT"]],
                                       with_welcome = T,
                                       with_finish = T,
                                       feedback = EDT::EDT_feedback_with_graph())
                            )),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(
        "You can close the browser tab now."
      )), dict = dict)
  )
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   enable_admin_panel = T,
                                   admin_password = admin_password,
                                   demo = TRUE,
                                   languages = languages,
                                   #logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dots_logo_v3.png",
                                   logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/dgm_logo_v2.png",
                                   logo_width = "96px",
                                   logo_height = "auto"))
}