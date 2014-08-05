PROGRAM emisiones_etrome
IMPLICIT NONE
!    ProOsito:
!        Calcula las emisiones vehiculares usando la informaciOn de tramos, celdas, tipo de vehIculo y factores de emisiOn
!    ParAmetros:
!        Directorio de los archivos de entrada
!        No puede contener caracteres especiales
!    Entradas:
!        Matriz de flujos conocidos y calculados
!    Salida:
!        Matriz 
!    Dependencias:
!
!    CompilaciOn
!        Serial:
!        gfortran -Wall -O3 -o "emisiones_etrome" "emisiones_etrome.f90"
!            probado con gfortran 4:4.7.3-1ubuntu10 en Ubuntu 13.04
!        Paralelo:
!        gfortran -Wall -O3 -fopenmp -o "emisiones_etrome" "emisiones_etrome.f90"
!            probado con gfortran 4:4.7.3-1ubuntu10 en Ubuntu 13.04
!    
!    Historial de revisiones
!        Fecha        Autor            DescripciOn
!        ====        ==========        =====================
!        2013/08/26  Pablo García      Original

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de parAmetros
    INTEGER :: hours=24                                  !Horas en un dia

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de variables
!    Variables leidas
    INTEGER :: num_roads                                 !NUmero de tramos de la malla vial
    INTEGER :: num_veh_ids                               !NUmero de tipos de vehIculos
    INTEGER :: species                                   !NUmero de especies (contaminantes emitidos)
    INTEGER :: cell_road_pairs                           !NUmero de combinaciones celda-vIa
    INTEGER :: num_cells                                 !NUmero de celdas del dominio

    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_flows       !Matriz de flujos vehiculares, dim [num_roads, hours]
    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_f_emiss     !Matriz de factores de emisiOn, dim [num_veh_ids, species]
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mat_cell_road_comb   !Matriz de combinaciones celda-vIa, dim [cell_road_pairs, 2]
    
    REAL, ALLOCATABLE, DIMENSION(:) :: vec_road_length   !Vector de longitudes de tramo, dim [num_roads]
    REAL, ALLOCATABLE, DIMENSION(:) :: vec_veh_dist      !Vector de distribuciOn del parque automotor, dim [num_veh_ids]
    
!    Variables calculadas
    INTEGER :: active_cells                              !NUmero de celdas activas

    REAL, ALLOCATABLE, DIMENSION(:) :: vec_cell_road_length !Matriz de longitod de vIa por celda, dim [cell_road_pairs]
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: mat_road_emiss!Matriz de emisiones por vIa, dim [hours,num_roads,species]
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: mat_cell_emiss  !Matriz de emisiones por celda para una hora, dim [active_cells,species]
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: mat_veh_emiss   !Matriz de emisiones por tipo de vehIculo para una hora, dim [hours,num_veh_ids,species]
    
    INTEGER, ALLOCATABLE, DIMENSION(:) :: vec_active_cells  !Vector de celdas activas, 0 o 1, dim [num_cells]
    INTEGER, ALLOCATABLE, DIMENSION(:) :: vec_cell_id    !Vector de identificadores de celda, dim [active_cells]

!    *   *   *   *
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: mat_emiss   !Matriz 4D de emisiones por combinaciOn celda-tramo, contaminante, hora y
!                                                        !tipo de vehIculo, dim [cell_road_pairs, hours, species, num_veh_ids]
!    *   *   *   *
    
!    Variables de control
    INTEGER :: readstatus                                !Estado de la instrucciOn de lectura
    INTEGER :: allocate_status                           !Estado de la instrucciOn de asignaciOn de memoria
    
    CHARACTER(LEN=256) :: path                           !Directorio de los archivos de entrada
    CHARACTER(LEN=20) :: str_fout                        !Nombre del archivo de salida

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de variables para saltar campos
    INTEGER :: idum                                      !Variable de salto para enteros

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de contadores
    INTEGER :: arg_num, i, j, k, l, m                             !Integer counters
    INTEGER :: cell_id

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Captura de argumentos de entrada
    arg_num = COMMAND_ARGUMENT_COUNT()
    IF (arg_num == 0) THEN
        WRITE(*,*) "Es necesario proporcionar un directorio de los archivos de entrada"
        STOP
    ELSEIF (arg_num /= 1) THEN
        WRITE(*,*) "NUmero incorrecto de argumentos"
        STOP
    ELSE
!        Capturar el tipo de argumento
        CALL GET_COMMAND_ARGUMENT(1,path)
        WRITE(*,*) "Directorio de los archivos de entrada: ", path
    END IF

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de la matriz de flujos, contiene el vector de longitudes de vIa
!    Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de la matriz flujos, contiene el vector de longitudes de vIa'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    OPEN (UNIT = 7, FILE = TRIM(ADJUSTL(path))//"flujos.csv", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    
!    Saltar el encabezado del archivo
    READ (7,*,IOSTAT=readstatus)
    IF (readstatus > 0) THEN
        !Error de lectura
        WRITE (*,*) 'Error en la lectura'
        CLOSE (7)
        STOP
    END IF
    
!     Lectura del nUmero de tramos
    READ (7,*,IOSTAT=readstatus) num_roads, idum
    
    IF (readstatus > 0) THEN
        !    Error de lectura
        WRITE (*,*) 'No es posible leer el nUmero de tramos'
        CLOSE (7)
        STOP
    ELSE
        WRITE (*,*) 'El nUmero de tramos es ', num_roads
    END IF

!    Asignar memoria a la matriz de flujos
    ALLOCATE (mat_flows(num_roads,hours), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Asignar memoria al vector de longitudes de vIa
    ALLOCATE (vec_road_length(num_roads), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Lectura de la matriz de flujos y el vector de longitudes de vIa
    WRITE (*,*) 'Matriz de flujos y vector de longitudes de vIa'
    DO i = 1, num_roads
!        WRITE (*,*) 'Leyendo la vIa ', i
        READ (7,*,IOSTAT=readstatus) idum, idum, vec_road_length(i), (mat_flows(i,j), j = 1, hours)
        IF (readstatus > 0) THEN
            !    Error de lectura
            WRITE (*,*) 'Error de lectura'
            CLOSE (7)
            STOP
        END IF
    END DO
    CLOSE (7)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de la matriz de factores de emision, contiene la distribuciOn del parque automotor
!    Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de factores de emision, contiene la distribuciOn del parque automotor'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    OPEN (UNIT = 7, FILE = TRIM(ADJUSTL(path))//"factores_emision.csv", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    
!    Saltar el encabezado del archivo
    READ (7,*,IOSTAT=readstatus)
    IF (readstatus > 0) THEN
        !Error de lectura
        WRITE (*,*) 'Error en la lectura'
        CLOSE (7)
        STOP
    END IF
    
!     Lectura del nUmero de tipos de vehiculos y especies
    READ (7,*,IOSTAT=readstatus) num_veh_ids, species
!    Restar las dos primeras columnas para obtener el nUmero real de especies
    species = species - 2
    
    IF (readstatus > 0) THEN
        !    Error de lectura
        WRITE (*,*) 'No es posible leer el nUmero de tipo de vehiculos o especies'
        CLOSE (7)
        STOP
    ELSE
        WRITE (*,*) 'El nUmero de tipo de vehiculos es ', num_veh_ids
        WRITE (*,*) 'El nUmero de especies es ', species
    END IF

!    Asignar memoria a la matriz de factores de emisiOn
    ALLOCATE (mat_f_emiss(num_veh_ids,species), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Asignar memoria al vector de distribuciOn del parque automotor
    ALLOCATE (vec_veh_dist(num_veh_ids), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Lectura de la matriz de factores de emision, contiene la distribuciOn del parque automotor
    WRITE (*,*) 'Matriz de factores de emision y distribuciOn del parque automotor'
    DO i = 1, num_veh_ids
!        WRITE (*,*) 'Leyendo el tipo de vehIculo ', i
        READ (7,*,IOSTAT=readstatus) idum, vec_veh_dist(i), (mat_f_emiss(i,j), j = 1, species)
        IF (readstatus > 0) THEN
            !    Error de lectura
            WRITE (*,*) 'Error de lectura'
            CLOSE (7)
            STOP
        END IF
    END DO
    CLOSE (7)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de vIas y celdas
!    Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de vIas y celdas'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    OPEN (UNIT = 7, FILE = TRIM(ADJUSTL(path))//"codigo_etrome.csv", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    
!    Saltar el encabezado del archivo
    READ (7,*,IOSTAT=readstatus)
    IF (readstatus > 0) THEN
        !Error de lectura
        WRITE (*,*) 'Error en la lectura'
        CLOSE (7)
        STOP
    END IF
    
!     Lectura del nUmero de combinaciones celda-vIa
    READ (7,*,IOSTAT=readstatus) cell_road_pairs, idum, num_cells
    IF (readstatus > 0) THEN
        !    Error de lectura
        WRITE (*,*) 'No es posible leer el nUmero combinaciones o celdas'
        CLOSE (7)
        STOP
    ELSE
        WRITE (*,*) 'El nUmero de combinaciones celda-vIa es ', cell_road_pairs
        WRITE (*,*) 'El nUmero de celdas del dominio es ', num_cells
    END IF

!    Asignar memoria a la matriz de combinaciones celda-vIa y el vector de longitud de tramos
    ALLOCATE (mat_cell_road_comb(cell_road_pairs,2), vec_cell_road_length(cell_road_pairs), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
!    Lectura de la matriz de combinaciones celda-vIa, lectura de los porcentajes de vIa por celda
    DO i = 1, cell_road_pairs
!                                          celda                    vIa                      porcentaje
        READ (7,*,IOSTAT=readstatus) idum, mat_cell_road_comb(i,1), mat_cell_road_comb(i,2), vec_cell_road_length(i)
        IF (readstatus > 0) THEN
            !    Error de lectura
            WRITE (*,*) 'No es posible leer el nUmero combinaciones o celdas'
            CLOSE (7)
            STOP
        END IF
!        CAlculo de la longitud de tramo
!                                 convertir a fracciOn             multiplicar por la longitud de vIa
        vec_cell_road_length(i) = vec_cell_road_length(i) / 100. * vec_road_length(mat_cell_road_comb(i,2))
    END DO

!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (vec_road_length)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    CAlculo de emisiones
!    Asignar memoria a la matriz de emisiones
!                       i               j     k       l
    ALLOCATE (mat_emiss(cell_road_pairs,hours,species,num_veh_ids), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
    WRITE (*,*) "CAlculo de la matriz de emisiones"
!    Pragmas de OMP para paralelizaciOn local
!    Compilar con la bandera -fopenmp
    !$OMP PARALLEL SHARED(mat_emiss,mat_f_emiss,vec_cell_road_length,vec_veh_dist,mat_flows,mat_cell_road_comb)
    !$OMP DO SCHEDULE(DYNAMIC)
!    Loop de horas
    DO j = 1, hours
    WRITE (*,*) 'Procesando la hora ', j
!        Loop de combinaciones celda-vIa
        DO i = 1, cell_road_pairs
!            Loop de especies
            DO k = 1, species
!                Loop de tipos de vehIculo
                DO l = 1, num_veh_ids
!                                        factor emision     longitud de tramo         porcentaje de tipo de vehIculo
                    mat_emiss(i,j,k,l) = mat_f_emiss(l,k) * vec_cell_road_length(i) * vec_veh_dist(l) * &
!                    flujo vehicular
                    &mat_flows(mat_cell_road_comb(i,2),j)
                END DO
            END DO
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL

!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (mat_flows,mat_f_emiss,vec_veh_dist,vec_cell_road_length)
    

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Producir salida por vIas

!    AsignaciOn de unidades y apertura de archivos
    DO i = 1, hours
!        GeneraciOn del nombre de archivo
        WRITE(str_fout,"(A12,I2.2,A4)") "etrome_vIas_", i, ".csv"
        OPEN (UNIT = i + 6, FILE = TRIM(ADJUSTL(path))//str_fout, FORM = 'FORMATTED')
    END DO

!    Asignar memoria a la matriz de emisiones por vIa para una hora
    ALLOCATE (mat_road_emiss(hours,num_roads,species), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
!    Inicializar la matriz de emisiones por vIa para una hora en ceros
    mat_road_emiss = 0

    WRITE (*,*) "Escribiendo los archivos de emisiones por vIa"
!    Pragmas de OMP para paralelizaciOn local
!    Compilar con la bandera -fopenmp
    !$OMP PARALLEL
    !$OMP DO SCHEDULE(DYNAMIC)
    DO i = 1, hours
        WRITE (*,*) "Escribiendo la hora", i

!        CAlculo de emisiones por vIa
        DO j = 1, cell_road_pairs
!                          -----------vIa---------
            mat_road_emiss(i,mat_cell_road_comb(j,2),:) = SUM(mat_emiss(j,i,:,:),2) + &
!            TErmino de acumulaciOn
            &mat_road_emiss(i,mat_cell_road_comb(j,2),:)
        END DO
        
!        Escritura del archivo de emisiones por hora
        DO k = 1, num_roads
!            WRITE (*,*) 'Escribiendo la vIa ', k
            WRITE (i + 6,*) i, k, (mat_road_emiss(i,k,l), l = 1, species)
        END DO
                
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
!    Cierre de archivos y unidades
    DO i = 1, hours
        CLOSE (i + 6)
    END DO

!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (mat_road_emiss)


!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Producir salida por tipo de vehIculo
!    Asignar memoria a la matriz de emisiones por tipo de vehIculo
    ALLOCATE (mat_veh_emiss(hours,num_veh_ids,species), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
!    Inicializar la matriz de emisiones por vIa en ceros
    mat_veh_emiss = 0

!    AsignaciOn de unidades y apertura de archivos
    DO i = 1, hours
!        GeneraciOn del nombre de archivo
        WRITE(str_fout,"(A11,I2.2,A4)") "etrome_veh_", i, ".csv"
        OPEN (UNIT = i + 6, FILE = TRIM(ADJUSTL(path))//str_fout, FORM = 'FORMATTED')
    END DO

    WRITE (*,*) "Escribiendo los archivos de emisiones por tipo de vehIculo"
!    Pragmas de OMP para paralelizaciOn local
!    Compilar con la bandera -fopenmp
    !$OMP PARALLEL
    !$OMP DO SCHEDULE(DYNAMIC)
    DO i = 1, hours
        WRITE (*,*) "Escribiendo la hora", i

!        CAlculo de emisiones por tipo de vehIculo
        mat_veh_emiss(i,:,:) = TRANSPOSE(SUM(mat_emiss(:,i,:,:),1))
        
!        Escritura del archivo de emisiones por hora
        DO k = 1, num_veh_ids
            WRITE (i + 6,*) i, k, (mat_veh_emiss(i,k,l), l = 1, species)
        END DO
        
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
!    Cierre de archivos y unidades
    DO i = 1, hours
        CLOSE (i + 6)
    END DO

!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (mat_veh_emiss)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Producir salida por celdas

!    CAlculo del nUmero de celdas activas y el vector de identificadores de celdas
!    Asignar memoria al vector de celdas activas
    ALLOCATE (vec_active_cells(num_cells), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Inicializar el vector de celdas activas en cero
    vec_active_cells = 0
!    Insertar un 1 en cada celda activa
    DO i = 1, cell_road_pairs
        vec_active_cells(mat_cell_road_comb(i,1)) = 1
    END DO
    active_cells = SUM(vec_active_cells)
    WRITE (*,*) 'El nUmero de celdas activas es ', active_cells
!    Reemplazar los 1s por identificadores de celda
    cell_id = 1
    DO i = 1, num_cells
        IF (vec_active_cells(i) == 1) THEN
            vec_active_cells(i) = cell_id
            cell_id = cell_id + 1
        END IF
    END DO
    
 !    Asignar memoria al vector de identificadores de celdas
    ALLOCATE (vec_cell_id(active_cells), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
    cell_id = 1
    DO i = 1, num_cells
        IF (vec_active_cells(i) /= 0) THEN
            vec_cell_id(cell_id) = i
            cell_id = cell_id + 1
        END IF
    END DO

!    AsignaciOn de unidades y apertura de archivos
    DO i = 1, hours
!        GeneraciOn del nombre de archivo
        WRITE(str_fout,"(A14,I2.2,A4)") "etrome_celdas_", i, ".csv"
        OPEN (UNIT = i + 6, FILE = TRIM(ADJUSTL(path))//str_fout, FORM = 'FORMATTED')
    END DO
    
!    Asignar memoria a la matriz de emisiones por celda para una hora
    ALLOCATE (mat_cell_emiss(hours,active_cells,species), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
!    Inicializar la matriz de emisiones por celda para una hora en ceros
    mat_cell_emiss = 0

    WRITE (*,*) "Escribiendo los archivos de emisiones por celda"
!    Pragmas de OMP para paralelizaciOn local
!    Compilar con la bandera -fopenmp
    !$OMP PARALLEL
    !$OMP DO SCHEDULE(DYNAMIC)
    DO i = 1, hours
        WRITE (*,*) "Escribiendo la hora", i

!        CAlculo de emisiones por celda
        DO j = 1, cell_road_pairs
!                          --id de celda---(---------celda--------))
            mat_cell_emiss(i,vec_active_cells(mat_cell_road_comb(j,1)),:) = SUM(mat_emiss(j,i,:,:),2) + &
!            TErmino de acumulaciOn
            &mat_cell_emiss(i,vec_active_cells(mat_cell_road_comb(j,1)),:)
        END DO
        
!        Escritura del archivo de emisiones por hora
!       Contador para bUsqueda del id de celda
        m = 1
!       Bucle de celdas activas
        DO k = 1, active_cells
            WRITE (i + 6,*) i, vec_cell_id(k), (mat_cell_emiss(i,k,l), l = 1, species)
        END DO
        
    END DO
    !$OMP END DO
    !$OMP END PARALLEL

!    Cierre de archivos y unidades
    DO i = 1, hours
        CLOSE (i + 6)
    END DO
    
!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (mat_cell_emiss)

!    Retirar de la memoria las variables que no se utilizarAn mAs
    DEALLOCATE (mat_cell_road_comb,vec_active_cells,vec_cell_id)

STOP
END PROGRAM emisiones_etrome
