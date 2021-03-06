#' Read Brazilian 2010 Census data from IBGE website
#'
#' Fetch Brazilian 2010 Census data from ftp webpage of IBGE
#'
#' @param data data type to be retrieved from IBGE ( person, mortality, household, emigration )
#' @param vars_list list of variables (characters) to be retrieved from microdata
#' @param vars_newnames (optional) new names for selected variables
#' @param municode numerical code of municipality (6 digit)
#' @param state state abbreviate name (2 letters character)
#' @param save_RData set TRUE to save data in dest_folder as RData
#' @param save_csv set TRUE to save data in dest_folder as csv (be careful with sample size)
#' @param format_microdata (optional) set TRUE to format microdata according to census layout
#' @param dest_folder destination folder to save microdata
#'
#' @return selected states Brazil 2010 census microdata
#' @export
#' @source
#' Censo Demográfico. 2010. Instituto Brasileiro de Geografia e Estatística (IBGE).
#'
#' @examples

#'

read_ibge_census2010 <-
  function(
    data = 'person', # person, mortality, household, emigration
    vars_list = NULL,
    vars_newnames = NULL,
    municode = NULL,
    state = 'all',
    save_RData = F,
    save_csv = F,
    format_microdata = F,
    dest_folder = NULL
  ){

    require( utils )
    require( readr )

    sel_data <- ifelse( data == 'person',
                        'pessoas',
                        ifelse( data == 'mortality',
                                'mortalidade',
                                ifelse( data == 'household',
                                        'domicilios',
                                        'emigracao' ) ) )
    state <- toupper( state )

    ftpibge2010 <- 'ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/'

    ufcodesftp <-
      c( 'AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS',
         'MT', 'PA', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE',
         'SP1', 'SP2_RM', 'TO' )

    if( state == 'ALL' ){
      uffilelist <- ufcodesftp
    } else{

      if( sum( grepl( 'SP', state ) ) != 0 ){
        uffilelist <- ufcodesftp[ ufcodesftp %in% state | grepl( 'SP', ufcodesftp ) ]
      } else{
        uffilelist <- ufcodesftp[ ufcodesftp %in% state ]
      }

    }

    tempd <- tempdir() # tempdir

    for( uf in uffilelist ){
      zipfile <- paste0( ftpibge2010, uf, '.zip' )
      tempf <- tempfile() # tempfile
      download.file( zipfile, tempf, method = 'curl' )
      unzip( tempf, exdir = tempd )
      gc( reset = T )
    }

    if( data == 'person' ){
      sel_vars <-
        person_layout_brcensus2010 %>%
        filter( varcode %in% vars_list )
    } else if( data == 'household'){
      sel_vars <-
        houshld_layout_brcensus2010 %>%
        filter( varcode %in% vars_list )
    } else if( data == 'emigration'){
      sel_vars <-
        emigr_layout_brcensus2010 %>%
        filter( varcode %in% vars_list )
    } else{
      sel_vars <-
        mort_layout_brcensus2010 %>%
        filter( varcode %in% vars_list )
    }


    # uf dirs
    dir_ufs <-
      file.path(
        tempd,
        list.files( file.path( tempd ) )[ list.files( file.path( tempd ) ) %in% ufcodesftp ]
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

    microdata <-
      do.call( rbind, microdata_list )

    if( !is.null( vars_newnames ) ){
      names( microdata ) <- var_newnames
    }

    if( format_microdata ){
      # add function to adjust format of variables
    }

    unlink( tempd )
    return( microdata )
  }


#read_ibge_census2010( vars_list = c( 'V0001', 'V0620', 'V6222'), state = c('sp'))
