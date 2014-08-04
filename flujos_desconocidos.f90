PROGRAM flujos_desconocidos
IMPLICIT NONE
!    ProOsito:
!        Lee la matriz de entradas y salidas y la matriz de flujos y las multiplica
!    Entradas:
!        Matriz de entradas y salidas en texto separado por espacios
!        Matriz de flujos en texto separado por espacios
!    Salida:
!        Matriz multiplicada en texto separado por espacios
!    Dependencias:
!        lapack
!            probado con liblapack-dev 3.4.2-1~exp3 en Ubuntu
!            compilar con la opciOn -llapac
!    CompilaciOn
!        gfortran -Wall -o "flujos_desconocidos" "flujos_desconocidos.f90" -llapack
!            probado con gfortran 4:4.7.3-1ubuntu10 en Ubuntu 13.04
!    
!    Historial de revisiones
!        Fecha        Autor            DescripciOn
!        ====        ==========        =====================
!        2013/08/22    Pablo García    Original

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de parAmetros
    INTEGER :: hours=24                                    !Horas en un dia

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de variables
    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_nodes    !Matriz de entradas y salidas
    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_LU       !Factores L U de la mat_nodes factorizada
    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_flow     !Matriz de flujos
    REAL, ALLOCATABLE, DIMENSION(:,:) :: mat_out      !Matriz de salida
    
    INTEGER, ALLOCATABLE, DIMENSION(:) :: vec_pivot      !Vector de pivotes para SGESV
    
    INTEGER :: nodes                                     !NUmero de nodos de la malla vial
                                                         !mat_nodes es nodes x nodes, mat_flow es hours x nodes
    
    INTEGER :: readstatus                                !Estado de la instrucciOn de lectura
    INTEGER :: allocate_status                           !Estado de la instrucciOn de asignaciOn de memoria
    INTEGER :: sgesv_status                              !Estado de la soluciOn con sgesv 
    
!    CHARACTER(LEN=256) :: filename                       !Nombre de los archivos de entrada

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de contadores
    INTEGER :: i, j                                      !Integer counters

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Rutinas externas
    EXTERNAL SGESV

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de la matriz de entradas y salidas    
!     Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de la matriz de entradas y salidas'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    OPEN (UNIT = 7, FILE = "matrizA.dat", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    
    READ (7,*,IOSTAT=readstatus) nodes
    IF (readstatus > 0) THEN
        !    Error de lectura
        WRITE (*,*) 'No es posible leer el nUmero de nodos'
        CLOSE (7)
        STOP
    ELSE
        WRITE (*,*) 'El nUmero de nodos es ', nodes
    END IF

!    Asignar memoria a la matriz de nodos y la matriz de factores LU
    ALLOCATE (mat_nodes(nodes,nodes), mat_LU(nodes,nodes), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Lectura de la matriz de nodos    
    WRITE (*,*) 'Matriz de nodos'
    DO i = 1, nodes
!        WRITE (*,*) 'Leyendo la fila ', i
        READ (7,*,IOSTAT=readstatus) (mat_nodes(i,j), j = 1, nodes)
        IF (readstatus > 0) THEN
            !    Error de lectura
            WRITE (*,*) 'Error de lectura'
            CLOSE (7)
            STOP
        END IF
    END DO
    CLOSE (7)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de la matriz de flujos    
!     Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de la matriz de flujos'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')
    OPEN (UNIT = 7, FILE = "matrizB_2.dat", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')

!    Asignar memoria a la matriz de flujos y a la matriz de salida
    ALLOCATE (mat_flow(nodes,hours), mat_out(nodes,hours), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF

!    Lectura de la matriz de flujos    
    WRITE (*,*) 'Matriz de flujos'
    DO i = 1, nodes
!        WRITE (*,*) 'Leyendo el nodo ', i
        READ (7,*,IOSTAT=readstatus) (mat_flow(i,j), j = 1, hours)
        IF (readstatus > 0) THEN
            !    Error de lectura
            WRITE (*,*) 'Error de lectura'
            CLOSE (7)
            STOP
        END IF
    END DO
    CLOSE (7)

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    SoluciOn del sistema de ecuaciones
!    Clonar la matriz de entrada a la matriz factorizada
!    La rutina SGESV escribe la matriz de factores LU sobre la matriz de entrada
    mat_LU=mat_nodes
!    Clonar la matriz de flujos a la matriz de soluciOn
!    La rutina SGESV escribe la matriz soluciOn sobre la matriz de tErminos independientes
    mat_out=mat_flow

!    Asignar memoria al vector de pivotes    
    ALLOCATE (vec_pivot(nodes), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
!    SoluciOn del sistema de ecuaciones
    CALL SGESV( nodes, hours, mat_LU, nodes, vec_pivot, mat_out, nodes, sgesv_status )
    IF (sgesv_status == 0) THEN
        WRITE (*,*) 'La soluciOn del sistema de ecuaciones fue exitosa'
    ELSE IF (sgesv_status <= 0) THEN
        WRITE (*,*) 'El argumento ', ABS(sgesv_status), ' es invAlido'
        STOP
    ELSE IF (sgesv_status >= 0) THEN
        WRITE (*,*) 'El factor U es singular'
        STOP
    END IF

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    Abrir el archivo de la matriz de salida    
!     Este cOdigo es para ejecutar el prgrama interactivamente
!    WRITE (*,*) 'Ingrese la ruta del archivo de la matriz de salida'
!    READ (*,*) filename
!    WRITE (*,*) filename
 
!    OPEN (UNIT = 7, FILE = filename, FORM = 'FORMATTED')
    OPEN (UNIT = 7, FILE = "matrizX.dat", FORM = 'FORMATTED')
    DO i = 1, nodes
!        WRITE (*,*) 'Escribiendo el nodo ', i
        WRITE (7,*) (INT(ABS(mat_out(i,j))), j = 1, hours)
    END DO
    CLOSE (7)

STOP
END PROGRAM flujos_desconocidos
