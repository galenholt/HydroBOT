# THis is fixing the internal EWR parameter sheet and causal networks to add SDL units and also other issues encountered.


ret <- get_raw_ewrsheet()

thedups <- ret[duplicated(ret), ]

# I could just go for this, but want to look at what I'm doing first
ret2 <- ret[!duplicated(ret),]

ret |>
  dplyr::group_by(dplyr::across(tidyselect::everything())) |>
  dplyr::filter(dplyr::n()>1) |>
  dplyr::ungroup() |>
  dplyr::arrange(dplyr::across(tidyselect::everything()))


# readr::write_csv(ret2, 'parameter_sheet.csv', na = '', eol = '\r\n')


# now fix the causals
ce <- get_causal_ewr()

# wipe out the duplicates
ced <- purrr::map(ce, \(x) x[!duplicated(x), ])

# clean up the bits we want to join on to the ewr2Obj
retc <- cleanewrs(ret2) |>
  dplyr::select(planning_unit_name, gauge, ewr_code, ewr_code_timing, state, SWSDLName)
# There is duplication in this, due to differences in some of the rquirements. BUt we can delete it here because we just want unique linking to SWSDLNam
retc <- retc[!duplicated(retc),]

# I'm ending up with a lot of NA when I do this join.
e2o_test <- ced$ewr2obj |> dplyr::left_join(retc, relationship = 'many-to-one') # each row in the network should match to one swooslename
sum(is.na(e2o_test$SWSDLName))

# A few are because the network has ewr_codes that aren't in the ewr table
eco <- unique(e2o_test$ewr_code)
eto <- unique(retc$ewr_code)

eco[!eco %in% eto]
eto[!eto %in% eco]

sum(e2o_test$ewr_code %in% eco[!eco %in% eto])

# We can look into that further, but it's also a different problem

# TO move forwardc here, are PU and SWSDLs uniquely paired?

pusdl <- cleanewrs(ret2) |>
  dplyr::select(planning_unit_name, SWSDLName, l_t_w_p_short_name) |>
  dplyr::distinct()

pusdl |>
  dplyr::group_by(planning_unit_name) |>
  dplyr::filter(dplyr::n()>1) |>
  dplyr::ungroup() |>
  dplyr::arrange(planning_unit_name)

# so, I encountered this below, and that brought me up here. What I need to know now is whether these three catchments have the same links. ie does the same ewr --> env_obj --> target hold?

# When I have a gauge, it should become unique?
pusdlg <- cleanewrs(ret2) |>
  dplyr::select(planning_unit_name, gauge, SWSDLName) |>
  dplyr::distinct()

# that clears up the dups.
pusdlg |>
  dplyr::group_by(planning_unit_name, gauge) |>
  dplyr::filter(dplyr::n()>1) |>
  dplyr::ungroup() |>
  dplyr::arrange(planning_unit_name, gauge)


# There's an issue with naming the PUs
ced[1:2] <- ced[1:2] |> purrr::map(\(x) {
  x$planning_unit_name[x$planning_unit_name == 'Border rivers'] <- 'Border Rivers and Moonie Long-term watering plan'
  return(x)
  })

# and an issue with the gauge in the CLLMM
ced$ewr2obj$gauge[ced$ewr2obj$gauge == 'A42610002'] <- 'A4261002'

# Let's use that then when I have a gauge
e2o <- ced$ewr2obj |>
  dplyr::left_join(pusdlg, relationship = 'many-to-one') # each row in the network should match to one swooslename

# Fixing names gets us down to about 200 that don't match (though there are still a lot of pus in the table that arent' in the network). That might be OK now that we skip the PU step
sum(is.na(e2o$SWSDLName))

# the PU
eco <- unique(ced$ewr2obj$planning_unit_name)
eto <- unique(pusdlg$planning_unit_name)

eco[!eco %in% eto]
eto[!eto %in% eco]

sum(e2o$planning_unit_name %in% eco[!eco %in% eto])

# I fixed the Coorong in the parameter sheet, it was a typo. but I think I'll fix the Border rivers in the causal

# the gauges
eco <- unique(ced$ewr2obj$gauge)
eto <- unique(pusdlg$gauge)

eco[!eco %in% eto]
eto[!eto %in% eco]

# final tests
sum(e2o$gauge %in% eco[!eco %in% eto])
any(duplicated(e2o))

# Now we get to the objetive to target issue, where there are three sdls. we
# have to assume that the objective to target mapping works the same in all 3.
# That's likely reasonable, the detailed differences would come in at the
# ewr-objective most likely, and that is guage-linked.
# SO, that means the network will just expand by 3 into those three sdls.

pusdl <- pusdl |> dplyr::select(-l_t_w_p_short_name)

o2t <- ced$obj2target |>
  dplyr::left_join(pusdl, relationship = 'many-to-many') # I know there are triplicates, so just allow it.

# tests
sum(is.na(o2t$SWSDLName))
any(duplicated(o2t))

# The last sheet is not space-linked
o2yt <- ced$obj2yrtarget |> dplyr::select(-1)

# There are a number of typos, fix a few simple ones of double spaces, leading and trailing
e2o <- e2o |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('  ', ' ', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('^ ', '', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub(' $', '', x)))

o2t <- o2t |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('  ', ' ', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('^ ', '', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub(' $', '', x)))

o2yt <- o2yt |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('  ', ' ', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub('^ ', '', x))) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) gsub(' $', '', x)))

# WRITE OUT DATA
readr::write_csv(e2o, 'ewr2obj.csv', na = '', eol = '\r\n')
readr::write_csv(o2t, 'obj2target.csv', na = '', eol = '\r\n')
readr::write_csv(o2yt, 'obj2yrtarget.csv', na = '', eol = '\r\n')

# Old- worth exploring some of the issues with the networks --------------



# The obj2target is PU-reffed, but not gauge, and doesn't have ewr codes, so this will need to just link to SWSDLName by PU and state
retc2 <- cleanewrs(ret2) |>
  dplyr::select(planning_unit_name, state, SWSDLName)
# There is duplication in this, due to differences in some of the rquirements. BUt we can delete it here because we just want unique linking to SWSDLNam
retc2 <- retc2[!duplicated(retc2),]

# This (Warrego Paroo Bulloo and Nebine Long-term watering plan)
retc2 |> dplyr::filter(planning_unit_name == 'Warrego Paroo Bulloo and Nebine Long-term watering plan')
# maps to Nebine, Paroo, Warrego and so fails a many-to-one.

# The question then is, *should* it?
# in the original parameter sheet (before we remove gauge, ewr code), they're informed by different gauges
retc |> dplyr::filter(planning_unit_name == 'Warrego Paroo Bulloo and Nebine Long-term watering plan') |>
  dplyr::arrange(planning_unit_name, gauge, ewr_code, ewr_code_timing) |> View()

# if we remove gauge, the ewrs are identical
retc |> dplyr::filter(planning_unit_name == 'Warrego Paroo Bulloo and Nebine Long-term watering plan') |>
  dplyr::select(-gauge) |> dplyr::distinct() |>
  dplyr::arrange(planning_unit_name, ewr_code, ewr_code_timing) |> View()

# The question reall is whether all the ewr_codes map to the same obj in each of those SWSDLs? this is a q for e2o_test
e2o_test |> dplyr::filter(planning_unit_name == 'Warrego Paroo Bulloo and Nebine Long-term watering plan') |>
  dplyr::arrange(planning_unit_name, ewr_code, ewr_code_timing, env_obj) |> View()



o2t <- ced$obj2target |> dplyr::left_join(retc2, relationship = 'many-to-one')
