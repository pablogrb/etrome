PROGRAM multiplicacion_matrices
IMPLICIT NONE
!    ProOsito:
!        Lee la matriz de entradas y salidas y la matriz de flujos y las multiplica
!    Entradas:
!        Matriz de entradas y salidas en texto separado por espacios
!        Matriz de flujos en texto separado por espacios
!    Salida:
!        Matriz multiplicada en texto separado por espacios
!    Dependencias:
!        
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
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mat_inout    !Matriz de entradas y salidas
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mat_flow    !Matriz de flujos
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mat_out        !Matriz de salida
    
    INTEGER :: nodes                                    !NUmero de nodos de la malla vial
                                                        !mat_inout es nodes x nodes, mat_flow es hours x nodes
    
    INTEGER :: readstatus                                !Estado de la instrucciOn de lectura
    INTEGER :: allocate_status                            !Estado de la instrucciOn de asignaciOn de memoria
    
    CHARACTER(LEN=256) :: filename                        !Nombre de los archivos de entrada

!    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
!    DeclaraciOn de contadores
    INTEGER :: i, j, k                                    !Integer counters

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

    ALLOCATE (mat_inout(nodes,nodes), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
    DO i = 1, nodes
        WRITE (*,*) 'Leyendo la fila ', i
        READ (7,*,IOSTAT=readstatus) (mat_inout(i,j), j = 1, nodes)
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
    OPEN (UNIT = 7, FILE = "matrizB.dat", FORM = 'FORMATTED', STATUS = 'OLD', ACTION = 'READ')

    ALLOCATE (mat_flow(hours,nodes), STAT=allocate_status)
    IF (allocate_status /= 0) THEN
        WRITE (*,*) 'FallO la asignaciOn de memoria, memoria insuficiente'
        STOP
    END IF
    
    DO i = 1, hours
        WRITE (*,*) 'Leyendo la hora ', i
        READ (7,*,IOSTAT=readstatus) (mat_flow(i,j), j = 1, nodes)
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
    CALL SGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
    

STOP
END PROGRAM multiplicacion_matrices
