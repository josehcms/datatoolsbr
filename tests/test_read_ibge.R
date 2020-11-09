require(tidyverse)
require(data.table)
# read_ibge_census2010 <-
#   function(
#     data = 'person', # person, mortality, household, emigration
#     vars_list = NULL,
#     vars_newnames = NULL,
#     municode = NULL,
#     state = 'all',
#     save_RData = F,
#     save_csv = F,
#     format_microdata = F,
#     dest_folder = NULL
#   ){
#
#     require( utils )
#     require( readr )
#
#     sel_data <- ifelse( data == 'person',
#                         'pessoas',
#                         ifelse( data == 'mortality',
#                                 'mortalidade',
#                                 ifelse( data == 'household',
#                                         'domicilios',
#                                         'emigracao' ) ) )
#     state <- toupper( state )
#
#     ftpibge2010 <- 'ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/'
#
#     ufcodesftp <-
#       c( 'AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS',
#          'MT', 'PA', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE',
#          'SP1', 'SP2_RM', 'TO' )
#
#     if( state == 'ALL' ){
#       uffilelist <- ufcodesftp
#     } else{
#
#       if( sum( grepl( 'SP', state ) ) != 0 ){
#         uffilelist <- ufcodesftp[ ufcodesftp %in% state | grepl( 'SP', ufcodesftp ) ]
#       } else{
#         uffilelist <- ufcodesftp[ ufcodesftp %in% state ]
#       }
#
#     }
#
#     tempd <- tempdir() # tempdir
#
#     for( uf in uffilelist ){
#       zipfile <- paste0( ftpibge2010, uf, '.zip' )
#       tempf <- tempfile() # tempfile
#       download.file( zipfile, tempf, method = 'curl' )
#       unzip( tempf, exdir = tempd )
#       gc( reset = T )
#     }
#
#     sel_vars <-
#       person_layout_brcensus2010 %>%
#       filter( varcode %in% vars_list )
#
#     # uf dirs
#     dir_ufs <-
#       file.path(
#         tempd,
#         list.files( file.path( tempd ) )[ list.files( file.path( tempd ) ) %in% ufcodesftp ]
#       )
#
#     data_path_list <-
#       lapply( dir_ufs,
#               function( x ){
#                 data_path_list  <-
#                   file.path( x,
#                              list.files( x )[ grepl(sel_data,
#                                                     tolower( list.files( x ) ) ) ] )
#                 return( data_path_list )
#
#               } )
#
#     data_path_vec <-
#       do.call( c, data_path_list )
#
#     col_pos <-
#       fwf_positions(
#         sel_vars$start_pos,
#         sel_vars$end_pos,
#         col_names = sel_vars$varcode
#       )
#
#     microdata_list <-
#       lapply( data_path_vec,
#               function( x ){
#                 microdata <-
#                   data.table::as.data.table(
#                     readr::read_fwf(
#                       file = file.path( x ),
#                       col_positions = col_pos,
#                     )
#                   )
#                 return( microdata )
#               })
#
#     microdata <-
#       do.call( rbind, microdata_list )
#
#     if( !is.null( vars_newnames ) ){
#       names( microdata ) <- var_newnames
#     }
#
#     if( format_microdata ){
#       # add function to adjust format of variables
#     }
#
#     unlink( tempd )
#     return( microdata )
#   }

brdat1 <-
  read_ibge_census2010( data = 'household',
                        vars_list = c( 'V0001', 'V0010', 'V1002', 'V1006' ),
                      state = 'sp'
                      )

vars_list = c( 'V0001', 'V0010', 'V1002', 'V1006' )

ufcodesftp <-
  c( 'AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS',
     'MT', 'PA', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE',
     'SP1', 'SP2_RM', 'TO' )

sel_vars <-
  datatoolsbr::houshld_layout_brcensus2010 %>%
  filter( varcode %in% vars_list )

tempd = '/home/josehcms/CENSO'
sel_data = 'domicilios'
# uf dirs
dir_ufs <-
  file.path(
    tempd,
    list.files( file.path( tempd ) )[ list.files( file.path( tempd ) ) %in%
                                        ufcodesftp ]
  )

data_path_list <-
  lapply( dir_ufs,
          function( x ){
            data_path_list  <-
              file.path( x,
                         list.files( x )[ grepl(sel_data,
                                                tolower( list.files( x ) ) ) ] )
            return( data_path_list )

          } )

data_path_vec <-
  do.call( c, data_path_list )

col_pos <-
  fwf_positions(
    sel_vars$start_pos,
    sel_vars$end_pos,
    col_names = sel_vars$varcode
  )

microdata_list <-
  lapply( data_path_vec,
          function( x ){
            microdata <-
              data.table::as.data.table(
                readr::read_fwf(
                  file = file.path( x ),
                  col_positions = col_pos,
                )
              )
            return( microdata )
          })

brdat1 <-
  do.call( rbind, microdata_list )

brdat1$mesocod = paste0( brdat1$V0001, brdat1$V1002 )
brdat1$peso = as.numeric( brdat1$V0010 ) / 10^13

aux <-
  brdat1[, .( prop_rur = sum( peso[ V1006 == 2 ] ) / sum( peso ) ),
         .( mesocod ) ] %>%
  .[ , mesocod := as.numeric( mesocod ) ] %>%
  merge(
    fread('/home/josehcms/Downloads/OneDrive-2020-11-08/female_completeness_estimates.csv') %>%
      .[ year == 2010, ] %>%
      .[ , mesocod := as.numeric( code ) ] %>%
      .[ , f := as.numeric( gcggbseg ) ] %>%
      .[ , .( mesocod, f ) ],
    by = 'mesocod'
    ) %>%
  merge(
    fread('/home/josehcms/Downloads/OneDrive-2020-11-08/male_completeness_estimates.csv') %>%
      .[ year == 2010, ] %>%
      .[ , mesocod := as.numeric( code ) ] %>%
      .[ , m := as.numeric( gcggbseg ) ] %>%
      .[ , .( mesocod, m ) ],
    by = 'mesocod'
  )

require( ggplot2 )
aux[ , reg := ifelse( substr( mesocod, 1, 1 ) == 1 |
                        substr( mesocod, 1, 1 ) == 2,
                      'NNE',
                      ifelse( substr( mesocod, 1, 1 ) == 3 |
                                substr( mesocod, 1, 1 ) == 4,
                              'SSE',
                              'MW' ) ) ]

aux %>%
  melt(
    id.vars       = c( 'reg', 'mesocod', 'prop_rur' ),
    measure.vars  = c( 'f', 'm' ),
    value.name    = 'ggbseg',
    variable.name = 'sex'
  ) %>%
  .[, sex2 := ifelse( sex == 'f', 'Females', 'Males' ) ] %>%
  ggplot( ) +
  geom_point( aes( x = prop_rur, y = ggbseg, label = reg,
                   color = reg ) ) +
  geom_smooth( aes( x = prop_rur, y = ggbseg ),
               method = 'lm',
               size = 0.25,
               color = 'black',
               linetype = 'longdash',
               se = F ) +
  scale_x_continuous( breaks = seq( 0, 0.6, 0.1 ),
                      limits = c( 0, 0.6 ),
                      name = 'Proportion of rural population') +
  scale_y_continuous( breaks = seq( 0.3, 1, 0.05 ),
                      limits = c( 0.3, 1 ),
                      name = 'Death registration coverage (GGB-SEG)') +
  labs( title = 'Death registration coverage by mesorregion - Brazil, 2010' ) +
  facet_wrap( ~ sex2, nrow = 1 ) +
  theme_light()


dat <-
  aux %>%
  melt(
    id.vars       = c( 'reg', 'mesocod', 'prop_rur' ),
    measure.vars  = c( 'f', 'm' ),
    value.name    = 'ggbseg',
    variable.name = 'sex'
  )

cor.test( dat[ sex == 'f']$prop_rur, dat[ sex == 'f']$ggbseg )
cor.test( dat[ sex == 'm']$prop_rur, dat[ sex == 'm']$ggbseg )

plot( x = aux$prop_rur,  y = aux$gcggbseg )
abline( lm( aux$gcggbseg ~ aux$prop_rur ), col = 'red' )
cor.test( aux$gcggbseg, aux$prop_rur )
