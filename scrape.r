library(rvest)
library(tidyverse)
library(readr)
library(stringr)


# Set up your Open AI API key
Sys.setenv(OPENAI_API_KEY = "")

ask_gpt <- function(prompt, text, temperature = 0.5, top_p = 0.5) {
    print(Sys.time())
    response_each <- openai::create_chat_completion(
        # model = "gpt-4-1106-preview",
        model = "gpt-3.5-turbo-0125",
        temperature = temperature,
        top_p = top_p,
        messages = list(
            list(
                "role" = "system",
                "content" = prompt
            ),
            list(
                "role" = "user",
                "content" = text
            )
        )
    )
    return(response_each$choices$message.content)
}



# Link to your page

page <- read_html("https://openai.com/careers/search")

# Gett all the links to careers
full_page <- page |>
    html_nodes("a") |>
    html_attr("href") %>%
    tibble(link = .) |>
    filter(str_detect(link, "careers/")) |>
    mutate(
        full_link = paste0("https://openai.com", link),
        body = map(full_link, read_html)
    )



# this gets the description
full_page$body[[2]] |> html_nodes(".f-subhead-1") |> html_text()

# remove first row, it is a link to the career aggregator
ready_to_go <- full_page[-1, ] |>
    mutate(
        # There are 6 containers 4th has location, 6th main text
        main_text = map(body, \(x) html_nodes(x, ".ui-description") |> html_text()),
        location = map(body, \(x) html_nodes(x, ".f-subhead-1") |> html_text())
        # containers = map(body, \(x)html_nodes(x, ".container") |> html_text()),
        # This sometimes goes wrong though, but i am not sure :/
        # location = map_chr(containers, 4) |> str_remove("Careers"),
        # This one also sometimes goes off. You could filter to the largest text block
        # and get the main text that way
        # main_text = map_chr(containers, 6)
    )

# you might also prefilter on location, so as not to look at offers in japan,
# although why not go there?

# This is just to keep track of which one you have processed already, and not do double take if they have the same text in there
# Change TRUE to false if you want to ignore that
if (file.exists("last_scrape.rds") && TRUE) {
    last_scrape <- readr::read_rds("last_scrape.rds")

    links_to_update <- ready_to_go %>%
        left_join(last_scrape, by = "full_link") |>
        distinct() |>
        mutate(same = !(identical(main_text.x, main_text.y) && identical(location.x, location.y))) |>
        pull(same)
    readr::write_rds(select(ready_to_go, full_link, location, main_text), "last_scrape.rds")

    ready_to_go <- ready_to_go[links_to_update, ]
} else {
    readr::write_rds(select(ready_to_go, full_link, location, main_text), "last_scrape.rds")
}

# if nothing to update it will stop here
if(nrow(ready_to_go) == 0) {stop("Up to Date!")}

# Define you gpt prompts

check_prompt <- "You are expert evaluator of job postings. your goal is to answer questions based on the posting. You will be handsomely rewarded for correct evaluation. First you will see the posting, and after it you will see the questions along with instructions on how to output your answers.
"

your_questions <- "
name_of_value ; description ; example
experience_level ; years of experience required for the job ; 5 years/3 months/0 (in the case of no experience)
tech_abilities ; key technical abilities required for the job ; Large Language Models Training, Report creation, etc.
tech_stack ; what technologies do i need profficiency in ; Python, Cuda, Skitlearn, etc.
"

instructions <- "Answer these questions in the following format, so it can be read into R using read_delim, include the header!!!:
experience_level;tech_abilities;tech_stack \n
5 years;Model Valuation, model design, oop programming;R, Python, Cuda, statistical software
"

template_for_text <- "### JOB POSTING ### \\n {main_text} \\n \\n ### QUESTIONS ### \\n {your_questions} \\n \\n ### INSTRUCTIONS ### \\n {instructions}"


# This is for multiprocessing, so you can have multiple calls happen and the same time
# doing async in R is too much work, here i can just change one function
# Regular map also works fine not gonna lie
library(furrr)
plan(multisession, workers = 5)

gpt_response <- ready_to_go |>
    mutate(
        text_to_ask = str_glue(template_for_text),
        gpt_response = furrr::future_map(text_to_task, \(text) ask_gpt(prompt = check_prompt, text = text))
        # gpt_response = map(text_to_ask, \(text) ask_gpt(prompt = check_prompt, text = text)),
    )

# Read the responses
ready_to_save <- gpt_response |>
    mutate(
        csv_of_response = map(gpt_response, \(text)
        readr::read_delim(I(text),
            quote = "\"", delim = ";",
                col_types = c("c", "c", "c")
            # col_names = c("experience_level", "tech_abilities", "tech_stack")
        ))
    ) |>
    unnest(csv_of_response) |>
    transmute(
        last_updated = lubridate::today(),
        location,
        experience_level,
        `tech_abilities`,
        `tech_stack`,
        full_link
    )


# remove the links that were updated and add new one, rearrange based on the last update 
if (file.exists("for_jake.csv") && TRUE) {
    last_data <- readr::read_csv("for_jake.csv")
    ready_to_write <- last_data %>%
        filter(!full_link %in% ready_to_save$full_link) %>%
        bind_rows(ready_to_save) %>%
        arrange(last_updated)
} else {
    ready_to_write <- ready_to_save
}

readr::write_csv(ready_to_write, "for_jake.csv")
