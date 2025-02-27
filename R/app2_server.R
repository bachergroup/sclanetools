library(dplyr)
library(scLANE)
library(DT)
library(ggplot2)
library(shinyjs)
library(tidyr)

options(shiny.maxRequestSize=3000*1024^2)

#' @import dplyr
#' @import DT
#' @import scLANE
#' @import ggplot2
#' @import tidyr
#' @import shinyjs
#' @import tidyr
#' @import Seurat
#' @import ggpubr
#' @import gridExtra
#' @import gtable
#' @import grid
#' @import purrr
#' @export
visualizerModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Disable the upload button initially
    shinyjs::disable("uploadFile")

    # Enable the upload button when a file is uploaded
    observeEvent(input$rdsFileInput_Res, {
      shinyjs::enable("uploadFile")
    })
    observeEvent(input$rdsFileInput_Orig, {
      shinyjs::enable("uploadFile")
    })
    
    # Reactive object for the RDS file
    rdsObject <- reactive({
      req(input$rdsFileInput_Res, input$rdsFileInput_Orig) # Ensure file is provided
      object1 <- readRDS(input$rdsFileInput_Res$datapath)
      object2 <- readRDS(input$rdsFileInput_Orig$datapath)
      object1 <- list(results=object1)
      object <- append(object1, object2)
      
      geneTable <- makeTable(object) # this takes awhile
      geneOrder <- geneTable$Gene

      LIST = list(object, geneTable, geneOrder)
      names(LIST) <- c("Results", "geneTable", "geneOrder")
      
      return(LIST)
    })

    # Reactive value for the selected gene
    selectedGene <- reactiveVal(NULL)

    # Reactive for plot object
    plotInputObject <- reactive({
      in_data <- rdsObject()
    
      geneToPlot <- in_data$geneOrder[1] # Default to first gene
      if (!is.null(selectedGene())) {
        geneToPlot <- in_data$geneOrder[selectedGene()]
      }
      outplot <- customPlot(in_data$Results, 
                            geneToPlot, 
                            lineage = "A", log1p.norm=TRUE)
      return(outplot)
    })

    # Observe when a row is selected in the gene table
    observeEvent(input$geneTable_rows_selected, {
      selectedGene(input$geneTable_rows_selected)
    })

    # Render the plot
    output$genePlot <- renderPlot({
        req(plotInputObject())
        plotInputObject()
      },width = "auto")

    output$geneTable <- DT::renderDataTable({
      in_data <- rdsObject()
      
      geneDF <- in_data$geneTable
      
      # Render DataTable with left-aligned column header and values
      DT::datatable(geneDF,
        rownames = FALSE,
        selection = "single",
        options = list(
          autoWidth = TRUE,
          scrollX = TRUE,
          pageLength = 10, 
          columnDefs = list(
            list(className = "dt-left", targets = "_all") # Left-align both header and values
          )
        )
      ) %>% DT::formatStyle(
        columns = "Gene", fontSize = "16pt",
        textAlign = "left" # Left-align the text content
      ) %>% DT::formatStyle(
        columns = setdiff(names(geneDF), "Gene"), fontSize = "14pt",
        textAlign = "left" # Left-align the text content
      )
    })

    # Download handler for PDF export
    output$downloadPlot <- downloadHandler(
      filename = function() {
        in_data <- rdsObject()
        paste("gene_plot_", in_data$geneOrder[selectedGene()], ".pdf", sep = "")
      },
      content = function(file) {
        # Create the plot
        pdf(file, height = 5, width = 9)
        print(plotInputObject())
        dev.off()
      }
    )

    # Download handler for CSV export of the gene table
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("results_table_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        in_data <- rdsObject()
        geneDF <- in_data$geneTable
        write.csv(geneDF, file, row.names = FALSE)
      }
    )
  })
}


customPlot <- function(INDATA, gene, lineage, log1p.norm) {

  all_lineages <- gsub("Lineage_", "", names(INDATA$results[[1]]))
  if (length(all_lineages) == 1) {
    gfv_filter <- NULL
  } else {
    gfv_filter <- all_lineages[all_lineages != lineage]
  }
  fitted_vals <- scLANE::getFittedValues(INDATA$results, genes = gene, 
                               pt = INDATA$pt, 
                               expr.mat = INDATA$expr.mat, 
                               id.vec = INDATA$id.vec, 
                               is.gee=INDATA$is.gee,
                               size.factor.offset = INDATA$size.factor.offset, 
                               log1p.norm = log1p.norm, filter.lineage = gfv_filter)
  if (log1p.norm) {
    fitted_vals <- dplyr::select(fitted_vals, cell, lineage, model_offset,
                                 pt, gene, rna = rna_log1p, scLANE_pred = scLANE_pred_log1p, 
                                 scLANE_ci_ll = scLANE_ci_ll_log1p, scLANE_ci_ul = scLANE_ci_ul_log1p)
    # if (INDATA$is.gee) {## this is probably not needed and was only a versioning issue, remove later
    #   fitted_vals$scLANE_pred <- fitted_vals$scLANE_pred + mean(fitted_vals$model_offset)
    #   fitted_vals$scLANE_ci_ll <- fitted_vals$scLANE_ci_ll + mean(fitted_vals$model_offset)
    #   fitted_vals$scLANE_ci_ul <- fitted_vals$scLANE_ci_ul + mean(fitted_vals$model_offset)
    # }
    
  } else {
    fitted_vals <- dplyr::select(fitted_vals, cell, lineage, model_offset,
                               pt, gene, rna, scLANE_pred, scLANE_ci_ll, scLANE_ci_ul)
    # if (INDATA$is.gee) { ## this is probably not needed and was only a versioning issue, remove later
    #   fitted_vals$scLANE_pred <- fitted_vals$scLANE_pred * mean(exp(fitted_vals$model_offset))
    #   fitted_vals$scLANE_ci_ll <- fitted_vals$scLANE_ci_ll * mean(exp(fitted_vals$model_offset))
    #   fitted_vals$scLANE_ci_ul <- fitted_vals$scLANE_ci_ul * mean(exp(fitted_vals$model_offset))
    # }
    
  }
    knots <- unique(INDATA$results[[gene]][[paste0("Lineage_", lineage)]]$MARGE_Slope_Data$Breakpoint)
    dyn_plot <- ggplot2::ggplot(fitted_vals, ggplot2::aes(x = pt, y = rna)) + 
                ggplot2::geom_point(size = 2, alpha = 0.6, stroke = 0, color = "grey40") + 
                ggplot2::geom_vline(data = data.frame(gene = gene, knot = knots), 
                                    mapping = ggplot2::aes(xintercept = knot),
                                    linetype = "dashed", color = "black", linewidth = 1) + 
                ggplot2::geom_ribbon(ggplot2::aes(ymin = scLANE_ci_ll,  ymax = scLANE_ci_ul),
                                     linewidth = 0, fill = "#ABDDDE", alpha = 0.35) + 
                ggplot2::geom_line(ggplot2::aes(y = scLANE_pred), color = "#046C9A", linewidth = 2.5) + 
                ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) + 
                ggplot2::labs(x = "Pseudotime", y = ifelse(log1p.norm, "Log Expression", "Expression")) + 
                scLANE::theme_scLANE(base.size = 18) + 
                ggplot2::ggtitle(gene) + theme(axis.text = element_text(color = "black"))
  
    min_pt <- min(INDATA$pt[, which(LETTERS == lineage)], na.rm = TRUE)
    max_pt <- max(INDATA$pt[, which(LETTERS == lineage)], na.rm = TRUE)
    g.dynamics <- INDATA$results[[gene]][[paste0("Lineage_", lineage)]]$Gene_Dynamics
    n_breaks <- sum(grepl("Breakpoint", colnames(g.dynamics)))
    if (n_breaks == 1) {
      coef_sumy <- dplyr::select(g.dynamics, -dplyr::starts_with("Trend")) %>% 
                    tidyr::pivot_longer(dplyr::starts_with("Slope"), names_to = "Segment", values_to = "Coef") %>% 
                      dplyr::mutate(Breakpoint_Lag = dplyr::lag(Breakpoint), 
                                    Breakpoint_Lead = dplyr::lead(Breakpoint), 
                                    Interval = NA_character_, .before = 4) %>% 
                      dplyr::mutate(Breakpoint_Lag = dplyr::if_else(is.na(Breakpoint_Lag), min_pt, Breakpoint_Lag), 
                                    Breakpoint_Lead = dplyr::if_else(is.na(Breakpoint_Lead), max_pt, Breakpoint_Lead), 
                                    Interval = paste0("(", round(Breakpoint_Lag, 3), ", ", round(Breakpoint_Lead, 3), ")")) %>% 
                      dplyr::select(Interval, Coef) %>% dplyr::mutate(Coef = round(Coef, 3))
    
      coef_sumy_grob <- gridExtra::tableGrob(coef_sumy, cols = c("Interval", "Slope"), 
                                             theme = gridExtra::ttheme_minimal(base_size = 15, 
                                                                               core = list(fg_params = list(hjust = 0, x = 0.05)), 
                                                                               colhead = list(fg_params = list(hjust = 0, x = 0.05))), rows = NULL) %>% 
        gtable::gtable_add_grob(grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)), t = 1, b = nrow(.), l = 1, r = ncol(.)) %>% 
        gtable::gtable_add_grob(grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)), t = 1, l = 1, r = ncol(.))
      
      } else {
  
        breakpoints <- g.dynamics %>%
          select(starts_with("Breakpoint")) %>%
          pivot_longer(cols = everything(), names_to = "Breakpoint", values_to = "Value") %>%
          arrange(Value)
        
        g.dynamics_long <- g.dynamics %>%
          select(-starts_with("Trend")) %>%
          pivot_longer(
            cols = starts_with("Slope.Segment"),
            names_to = "Segment",
            values_to = "Coef"
          )
        
        g.dynamics_long <- g.dynamics_long %>%
          rowwise() %>%
          mutate(
            Segment_Number = as.numeric(gsub("Slope.Segment", "", Segment)),
            Breakpoint_Lag = case_when(
              Segment_Number == 1 ~ 0,
              Segment_Number > 1 ~ Segment_Number - 1,
              TRUE ~ NA_real_
            ),
            Breakpoint_Lead = case_when(
              Segment_Number <= nrow(breakpoints) ~ breakpoints$Value[Segment_Number],
              Segment_Number == nrow(breakpoints) + 1 ~ max_pt,
              TRUE ~ NA_real_
            ))
        g.dynamics_long$Breakpoint_Lag <- c(min_pt, breakpoints$Value[g.dynamics_long$Breakpoint_Lag])
        g.dynamics_long <- g.dynamics_long %>%
                              mutate(Interval = paste0("(", round(Breakpoint_Lag, 3), ", ", round(Breakpoint_Lead, 3), ")")) %>%
                              ungroup() %>% dplyr::select(Interval, Coef) %>% dplyr::mutate(Coef = round(Coef, 3))
        
        coef_sumy_grob <- gridExtra::tableGrob(g.dynamics_long, 
                                               cols = c("Interval", "Slope"), 
                                               theme = gridExtra::ttheme_minimal(base_size = 15, 
                                                                  core = list(fg_params = list(hjust = 0, x = 0.05)), 
                                                                  colhead = list(fg_params = list(hjust = 0, x = 0.05))), rows = NULL) %>% 
          gtable::gtable_add_grob(grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)), t = 1, b = nrow(.), l = 1, r = ncol(.)) %>% 
          gtable::gtable_add_grob(grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 3)), t = 1, l = 1, r = ncol(.))
      }

    dyn_plot_anno <- (ggpubr::ggarrange(dyn_plot, coef_sumy_grob, 
                                   ncol = 2, nrow = 1, widths = c(3, 1)))
   
   
return(dyn_plot_anno)
}


makeTable <- function(INDATA) {
  
  geneDF <- getResultsDErb(INDATA$results, fdr.cutoff=1) %>% 
    dplyr::select(Gene, Lineage, P_Val_Adj) %>% arrange(P_Val_Adj)
  geneDF <- geneDF %>% dplyr::mutate(P_Val_Adj = round(P_Val_Adj, 3))
  geneDF[geneDF==0] <- "< 0.001"
  
  numBreaks <- sapply(INDATA$results, function(x) length(
    grep("Breakpoint", names(purrr::map(x, "Gene_Dynamics")[[1]]))))
  
  numBreaks <- data.frame(
    Gene = names(numBreaks),
    NumBreaks = as.numeric(numBreaks)
  )
  geneDF <- geneDF %>%
    left_join(numBreaks, by = "Gene")
  
  
  summySlopes <- lapply(INDATA$results, function(x) {
    tempobj <- purrr::map(x, "MARGE_Slope_Data")[[1]]
    if (!is.null(tempobj)) {
      mydf <- data.frame(SlopePval = (tempobj[grep("P_Val", names(tempobj))]))
      mydf <- t(mydf)
      colnames(mydf) <- paste0("Slope.Pval", 1:ncol(mydf))
    }
    
    tempobj <- purrr::map(x, "Gene_Dynamics")[[1]]
    if (!is.null(tempobj)) {
      mydf2 <- data.frame(Slopes = tempobj[grep("Slope.Segment", names(tempobj))])
      colnames(mydf2) <- paste0("Slope.Segment", 1:ncol(mydf2))
    }
    
    if(exists("mydf2") & exists("mydf")) {
      newdf <- cbind(mydf, mydf2)
      rownames(newdf) <- tempobj$Gene
      return(newdf)
    } 
  })
  summySlopes <- bind_rows(summySlopes) 
  summySlopes <- round(summySlopes, 3)
  summySlopes$Gene <- rownames(summySlopes)
  
  geneDF <- geneDF %>%
    left_join(summySlopes, by = "Gene")
  return(geneDF)
}

getResultsDErb <- function(test.dyn.res = NULL,
                         p.adj.method = "fdr",
                         fdr.cutoff = 0.01) {
  # check inputs
  if (is.null(test.dyn.res)) { stop("Please provide a result list from testDynamic().") }
  if (!p.adj.method %in% stats::p.adjust.methods) { stop("Please choose a valid p-value adjustment method.") }
  
  # iterates first over genes, then over lineages per-gene & coerces to final data.frame after unlisting everything
  result_df <- lapply(test.dyn.res, function(x) {
    purrr::map_dfr(x, function(y) {
      as.data.frame(rbind(y[seq(15)])) %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), \(z) unname(unlist(z))))
    })
  }) 
  result_df <- bind_rows(result_df) 
  result_df <- result_df %>%
    dplyr::arrange(dplyr::desc(Test_Stat)) %>% 
    dplyr::mutate(P_Val_Adj = stats::p.adjust(P_Val, method = p.adj.method),
                  Gene_Dynamic_Lineage = dplyr::if_else(P_Val_Adj < fdr.cutoff, 1, 0, missing = 0)) %>%
    dplyr::with_groups(Gene, dplyr::mutate, Gene_Dynamic_Overall = max(Gene_Dynamic_Lineage)) %>% 
    dplyr::relocate(Gene, Lineage, Test_Stat, P_Val)
  
  return(result_df)
}

