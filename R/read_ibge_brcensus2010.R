
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
      download.file( zipfile, tempf )
      unzip( tempf, exdir = tempd )

      datafilepath <-
        file.path( file.path( tempd, uf ) ,
                   grep( data,
                         list.files( file.path( tempd, uf ) ),
                         value = T ) )

    }

    sel_vars <-
      person_layout_brcensus2010 %>%
      filter( varcode %in% vars_list )

    # uf dirs
    dir_ufs <-
      file.path(
        tempd,
        list.files( file.path( tempd ) )[ list.files( file.path( tempd ) ) %in% ufcodesftp ]
        )

    data_path_list <- c()

    for( uffile in dir_ufs ){
      data_path_list  <-
        c(
          data_path_list,
          file.path( uffile,
                     list.files( uffile )[ grepl(sel_data,
                                                 tolower( list.files( uffile ) ) ) ] ) )
    }


    col_pos <-
      fwf_positions(
        sel_vars$start_pos,
        sel_vars$end_pos,
        col_names = sel_vars$varcode
      )

    microdata <-
      data.frame()

    for( data_path in data_path_list ){
      microdata <-
        rbind(
          microdata,
          as.data.frame(
            read_fwf(
              file = file.path( data_path ),
              col_positions = col_pos,
              )
            )
          )
      }

    if( !is.null( vars_newnames ) ){
      names( microdata ) <- var_newnames
    }

    unlink( x = c( tempf, tempd ), recursive = T )

    if( format_microdata ){
      # add function to adjust format of variables
    }

    return( microdata )
  }


read_ibge_census2010( vars_list = c( 'V0001', 'V0620', 'V6222'), state = c('sp'))
