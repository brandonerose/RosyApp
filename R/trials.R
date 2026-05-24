#' @title Get Clinical Trial
#' @export
get_clinical_trial <- function(nct_id){
  #validate nctID ^[Nn][Cc][Tt]0*[1-9]\d{0,7}$
  base_url <- "https://clinicaltrials.gov/api/v2/studies/"
  response <- httr::GET(
    url = paste0(base_url,nct_id),
    query = list(
      format = "json",
      markupFormat = "markdown"
    )
  )
  httr::stop_for_status(response)
  content <- httr::content(response, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
  content
}
#' @title Generate Trial Diagram
#' @export
trial_diagram <- function(trial,
                          static = FALSE,
                          render = TRUE,
                          include_drugs = TRUE,
                          duplicate_drugs = FALSE,
                          hierarchical = TRUE,
                          direction = "LR",
                          zoomView = TRUE) {
  OUT <- create_node_edge_trial(
    trial,
    include_drugs = include_drugs,
    duplicate_drugs = duplicate_drugs
  )
  OUT$node_df$physics <- TRUE
  OUT$node_df$physics[which(OUT$node_df$group == "Trial")] <- FALSE
  if (static) {
    OUT$node_df$shape[which(OUT$node_df$shape == "box")] <- "rectangle"
    OUT$node_df$shape[which(OUT$node_df$shape == "ellipse")] <- "circle"
    colnames(OUT$node_df)[which(colnames(OUT$node_df) == "title")] <- "tooltip"
    colnames(OUT$node_df)[which(colnames(OUT$node_df) == "group")] <- "type"
    colnames(OUT$node_df)[which(colnames(OUT$node_df) == "color.border")] <- "color"
    colnames(OUT$node_df)[which(colnames(OUT$node_df) == "font.color")] <- "fontcolor"
    OUT$node_df$fillcolor <- OUT$node_df$color.background
    # node_df$color.highlight <- "gold"
    OUT$node_df$tooltip <- gsub("<br>", "\\\n", OUT$node_df$tooltip) |> remove_html_tags()
    if (is_something(OUT$edge_df))
      colnames(OUT$edge_df)[which(colnames(OUT$edge_df) == "width")] <- "penwidth"
    graph <- DiagrammeR::create_graph(nodes_df =  OUT$node_df,
                                      edges_df = OUT$edge_df)
    rendered_graph <- DiagrammeR::render_graph(graph,
                                               title = project$redcap$project_info$project_title,
                                               output = "graph")
  } else {
    bordercolor <- font.color <- "black"
    trial_color <- "lightblue"
    arm_color <- "green"
    drug_color <- "#FF474C"
    arrow_type <- "to"
    OUT$node_df$type <- OUT$node_df$group
    rendered_graph <- visNetwork::visNetwork(
      nodes =  OUT$node_df,
      edges = OUT$edge_df,
      height = "600px",
      main = list(
        text = trial$protocolSection$identificationModule$briefTitle,
        style = "font-size:24px;font-weight:bold;color:black;font-family:Georgia;text-align:center;"
      ),
      submain = list(
        text = paste0(
          "<br>Code by Brandon Rose, M.D., M.P.H. at <a href='https://www.thecodingdocs.com/home'>TheCodingDocs.com</a> using <a href='https://github.com/thecodingdocs/RosyREDCap'>RosyREDCap with REDCapSync</a> and <a href='https://github.com/datastorm-open/visNetwork'>VisNetwork</a>"
        ),
        style = "font-size:14px;color:black;font-family:Georgia;text-align:center;"
      )
    ) |>
      visNetwork::visInteraction(zoomView = zoomView) |>
      visNetwork::visOptions(highlightNearest = TRUE,
                             nodesIdSelection = TRUE) |>
      visNetwork::visGroups(
        groupname = "Trial",
        color = list(
          background = trial_color,
          border = bordercolor,
          highlight = trial_color
        ),
        font = list(color = bordercolor)
      ) |>
      visNetwork::visGroups(
        groupname = "Arm",
        color = list(
          background = arm_color,
          border = bordercolor,
          highlight = arm_color
        ),
        font = list(color = bordercolor)
      ) |>
      visNetwork::visGroups(
        groupname = "Drug",
        color = list(
          background = drug_color,
          border = bordercolor,
          highlight = drug_color
        ),
        font = list(color = bordercolor)
      ) |>
      visNetwork::visLegend(main = list(
        text ="Legend",
        style = "font-size:24px;font-weight:bold;color:black;font-family:Georgia;text-align:center;"
      )) |>
      visNetwork::visLayout(hierarchical = hierarchical)
    if (hierarchical) {
      rendered_graph <- rendered_graph |> visNetwork::visHierarchicalLayout(direction = direction, levelSeparation = 300L)
    }
    # if(include_fields){
    #   groups <- "Field"
    #   if(include_choices) groups <- groups |> append("Choice")
    #   rendered_graph <- rendered_graph |> visNetwork::visClusteringByGroup(groups = groups)
    # }
    # rendered_graph$x$options$groups <- rendered_graph$x$groups |> lapply(function(group){
    #   list(
    #     shape=OUT$node_df$shape[which(OUT$node_df$group==group)[[1]]],
    #     font = list(
    #       color = OUT$node_df$font.color[which(OUT$node_df$group==group)[[1]]]
    #     ),
    #     color = list(
    #       background = OUT$node_df$color.background[which(OUT$node_df$group==group)[[1]]],
    #       border = OUT$node_df$color.border[which(OUT$node_df$group==group)[[1]]]
    #     )
    #   )
    # }) |> unlist()
  }
  if (render) {
    return(rendered_graph)
  }
  graph
}
#' @title Generate Trial Diagram from NCT
#' @export
trial_diagram2 <- function(nct_id,
                           static = FALSE,
                           render = TRUE,
                           include_drugs = TRUE,
                           duplicate_drugs = FALSE,
                           hierarchical = TRUE,
                           direction = "LR",
                           zoomView = TRUE) {
  nct_id |>
    get_clinical_trial() |>
    trial_diagram(static = static,
                  render = render,
                  include_drugs = include_drugs,
                  duplicate_drugs = duplicate_drugs,
                  hierarchical = hierarchical,
                  direction = direction,
                  zoomView = zoomView)
}
#' @noRd
create_node_edge_trial <- function(trial,
                                   duplicate_drugs = TRUE,
                                   include_drugs = FALSE) {
  # setup ==========================
  node_df <- NULL
  edge_df <- NULL
  bordercolor <- font.color <- "black"
  trial_color <- "lightblue"
  arm_color <- "green"
  drug_color <- "#FF474C"
  arrow_type <- "to"
  armGroups <- trial$protocolSection$armsInterventionsModule$armGroups
  interventions <- trial$protocolSection$armsInterventionsModule$interventions
  # nodes ======================================================================
  # project ---------------------------------------------------------
  level <- 1L
  node_df <- node_df |> dplyr::bind_rows(
    data.frame(
      id = NA,
      group = "Trial",
      entity_name = trial$protocolSection$identificationModule$nctId,
      entity_label = trial$protocolSection$identificationModule$briefTitle,
      # label = forms$form_label |> stringr::str_replace_all( "[^[:alnum:]]", ""),
      level = level,
      title = trial$protocolSection$eligibilityModule$eligibilityCriteria,
      shape = "box",
      # entity
      style = "filled",
      color.background = trial_color,
      color.border = bordercolor,
      font.color = font.color,
      stringsAsFactors = FALSE
    )
  )
  level <- level + 1L
  node_df <- node_df |>
    dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "Arm",
        entity_name = armGroups$label,
        entity_label = armGroups$label,
        # turn to function
        level = level,
        # label = forms$form_label |> stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = armGroups$description,
        shape = "box",
        # entity
        style = "filled",
        color.background = drug_color,
        color.border = bordercolor,
        font.color = font.color,
        stringsAsFactors = FALSE
      )
    )
  # drugs -----------
  if (include_drugs) {
    if (duplicate_drugs) {
      level <- level + 1L
      node_df <- node_df |>
        dplyr::bind_rows(
          data.frame(
            id = NA,
            group = "Drug",
            entity_name = armGroups$interventionNames |> unlist(),
            entity_label = armGroups$interventionNames |> unlist(),
            # turn to function
            level = level,
            # label = forms$form_label |> stringr::str_replace_all( "[^[:alnum:]]", ""),
            title = armGroups$interventionNames |> unlist(),
            # match desc
            shape = "box",
            # entity
            style = "filled",
            color.background = drug_color,
            color.border = bordercolor,
            font.color = font.color,
            stringsAsFactors = FALSE
          )
        )
    } else {
      level <- level + 1L
      node_df <- node_df |>
        dplyr::bind_rows(
          data.frame(
            id = NA,
            group = "Drug",
            entity_name = interventions$name,
            entity_label = interventions$name,
            # turn to function
            level = level,
            # label = forms$form_label |> stringr::str_replace_all( "[^[:alnum:]]", ""),
            title = interventions$description,
            shape = "box",
            # entity
            style = "filled",
            color.background = drug_color,
            color.border = bordercolor,
            font.color = font.color,
            stringsAsFactors = FALSE
          )
        )
    }
  }
  # final nodes-------------------
  node_df$id <- seq_len(nrow(node_df))
  node_df$fixedsize <- FALSE
  # node_df$color.highlight <- "gold"
  node_df$label <- node_df$entity_label |>
    lapply(function(text) {
      wrap_text(text, 25L)
    }) |>
    unlist()
  rownames(node_df) <- NULL
  # edges ======================
  # trial to arms-------------------
  edge_df <- edge_df |> dplyr::bind_rows(
    data.frame(
      id = NA,
      from = node_df$id[which(node_df$group == "Trial")],
      to = node_df$id[which(node_df$group == "Arm")],
      rel = NA,
      #"Belongs to",
      style = "filled",
      color = font.color,
      arrowhead = "none",
      arrows = arrow_type,
      stringsAsFactors = FALSE
    )
  ) |> RosyUtils::all_character_cols()
  # events to forms ----------------------
  if (include_drugs) {
    if (duplicate_drugs) {
      #   sub_node_df <- node_df[which(node_df$group == "Form"), ]
      #   if (!all(sub_node_df$entity_name %in% event_mapping$form))
      #     stop("event match error! check the diagram function. For now do not duplicate forms.")
      #   edge_df <- edge_df |>
      #     dplyr::bind_rows(
      #       data.frame(
      #         id = NA,
      #         from = unlist(lapply(event_mapping$unique_event_name, function(x) {
      #           node_df$id[which(node_df$group == "Event" &
      #                              node_df$entity_name == x)]
      #         })),
      #         to = sub_node_df$id,
      #         rel = NA,
      #         #"Belongs to",
      #         style = "filled",
      #         color = font.color,
      #         arrowhead = "none",
      #         arrows = arrow_type,
      #         stringsAsFactors = FALSE
      #       )
      #     )
      # } else {
      #   edge_df <- edge_df |>
      #     dplyr::bind_rows(
      #       data.frame(
      #         id = NA,
      #         from = unlist(lapply(event_mapping$unique_event_name, function(x) {
      #           node_df$id[which(node_df$group == "Event" &
      #                              node_df$entity_name == x)]
      #         })),
      #         to =  unlist(lapply(event_mapping$form, function(x) {
      #           node_df$id[which(node_df$group == "Form" &
      #                              node_df$entity_name == x)]
      #         })),
      #         rel = NA,
      #         #"Belongs to",
      #         style = "filled",
      #         color = font.color,
      #         arrowhead = "none",
      #         arrows = arrow_type,
      #         stringsAsFactors = FALSE
      #       )
      #     )
      # }
    } else{
      from <- 1:nrow(armGroups) |> lapply(function(ROW) {
        rep(armGroups$label[[ROW]], length(armGroups$interventionNames[[ROW]]))
      }) |> unlist()
      to <- gsub("Drug: ", "", armGroups$interventionNames |> unlist())
      edge_df <- edge_df |> dplyr::bind_rows(
        data.frame(
          id = NA,
          from =       unlist(lapply(from, function(x) {
            node_df$id[which(node_df$group == "Arm" &
                               node_df$entity_name == x)]
          })),
          to = unlist(lapply(to, function(x) {
            node_df$id[which(node_df$group == "Drug" &
                               node_df$entity_name == x)]
          })),
          rel = NA,
          #"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type,
          stringsAsFactors = FALSE
        )|> RosyUtils::all_character_cols()
      )
    }
  }
  # final edges -------------------
  edge_df$id <- seq_len(nrow(edge_df))
  #repeating change -----------
  # node_df$repeating <- FALSE
  # f_tf <- node_df$entity_name %in% forms$form_name[which(forms$repeating)]
  # node_df$repeating[which(node_df$group == "Form" & f_tf)] <- TRUE
  # repeating_form_rows <- which(node_df$group == "Form" &
  #                                node_df$repeating)
  # node_df$group[repeating_form_rows] <- "Form (repeating)"
  # node_df$color.background[repeating_form_rows] <- repeating_form_color
  # if (project$metadata$is_longitudinal) {
  #   e_tf <- node_df$entity_name %in% events$event_name[which(events$repeating)]
  #   node_df$repeating[which(node_df$group == "Event" &
  #                             e_tf)] <- TRUE
  #   repeating_events_rows <- which(node_df$group == "Event" &
  #                                    node_df$repeating)
  #   node_df$group[repeating_events_rows] <- "Event (repeating)"
  #   node_df$color.background[repeating_events_rows] <- repeating_event_color
  # }
  # final --------------------
  OUT <- list(node_df = node_df, edge_df = edge_df)
  OUT
}
