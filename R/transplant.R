#' Transplant Candidates
#'
#' @param matches nested tibble of matches
#' @param rec_ids vector of c_id of transplanted candidates
#' @param max_offer maximum number of offers to consider
#'
#' @return a dataset of candidates that have been transplanted with their respective donors
#' @export
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr slice_head
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#'
#' @examples
#' mz <- match_las(syn_cands, syn_dons, wl_model = "LAS15", post_tx_model = "LAS15",
#'  wl_weight = 2, wl_cap = 365, post_tx_weight = 1, post_tx_cap = 365)
#' transplant_candidates(mz, rec_ids = NA, max_offer = NA)
transplant_candidates <- function(matches, rec_ids, max_offer = NA){

  rec_ids <- rec_ids
  new_ids <- NA
  tr <- tidyr::unnest(matches, cols = c(data)) |>  mutate(organs_rec = 0) |>  filter(d_id == -Inf) |> ungroup()

  for(i in 1:nrow(matches)){

    ## prevent candidates from getting transplanted twice
    d_of_org <- matches$data[[i]] |> filter(!(c_id  %in% rec_ids))

    d_org <- matches$don_org[i]

    if(d_org != "DLU"){

      d_of_org <- filter(d_of_org, surg_type != "D")
    }

    if(nrow(d_of_org) == 0){next}

    #### maximum number of offers to look at
    if(!is.na(max_offer)){
      max_o <- max_offer
      d_of_org <- matches$data[[i]] |>
        slice_head(n = max_o)
    }

    if(any(d_of_org$accept == 1)){

      d_of_org <- filter(d_of_org, accept == 1)

      of_x <- min(which(d_of_org$accept == 1))

      tx_o0 <- d_of_org[of_x, ]

      if(is.null(tx_o0$c_id)){next}
      if(is.na(tx_o0$c_id)){next}

      while((tx_o0$surg_type %in% c("S","E") & !tx_o0$match_single) | (tx_o0$surg_type %in% c("E","D") & !tx_o0$match_double)){

        if(tx_o0$surg_type %in% c("E","D") & tx_o0$match_double){break}
        if(tx_o0$surg_type %in% c("S","E") & tx_o0$match_single){break}

        d_of_org <- filter(d_of_org, c_id != tx_o0$c_id)

        if(nrow(d_of_org)==0){
          tx_o0 <- NULL
          break
        }

        of_x <- min(which(d_of_org$accept == 1))

        tx_o0 <- d_of_org[of_x, ]
      }

      if(is.null(tx_o0$c_id)){next}
      if(is.na(tx_o0$c_id)){next}

      org_rec <- if(d_org != "DLU"){1}else{2}

      tx_o <- d_of_org[of_x, ] |>
        bind_cols(matches[i, c("d_id", "don_org")]) |>
        mutate(organs_rec = org_rec)

      rec_ids <- c(rec_ids, as.numeric(tx_o[, "c_id"]))

      if(any(is.na(rec_ids))){
        rec_ids <- rec_ids |>
          na.omit() |>
          as.numeric()
      }

      tr <- bind_rows(tr, tx_o)

      ## This is looking at the person who just got it, if either, then not a match_double is correct
      if((tx_o$surg_type == "S" & tx_o$don_org == "DLU") | (tx_o$surg_type == "E" & tx_o$don_org == "DLU" & tx_o$match_single & !tx_o$match_double)){

        tr$organs_rec[nrow(tr)] <- 1

       ## Make sure we don't transplant the person twice
        d_of_org <- filter(d_of_org, !(c_id %in% rec_ids))

        if(nrow(d_of_org) == 0){next}

        if(all(!d_of_org$match_single)){next}
        ## Make sure they are able to receive the single offer
        d_of_org <- filter(d_of_org, match_single)

        if(all(d_of_org$surg_type == "D")){next}

        of_x2 <- min(which(d_of_org$accept==1 & d_of_org$surg_type %in% c("S", "E")))

        tx_o2 <- d_of_org[of_x2, ] |> bind_cols(matches[i, c("d_id", "don_org")])

        tr <- bind_rows(tr, tx_o2)

        tr$organs_rec[nrow(tr)] <- 1

        rec_ids <- c(rec_ids, tx_o2$c_id)

      }

    }else{
      of_x <- NULL
    }

  }

  return(tr)

}
