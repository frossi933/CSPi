#!/bin/bash
welcome="BIENVENIDO AL INTERPRETE DE CSP\n===============================\n\ningrese help para mas ayuda"
prompt=":>"
help[0]="loadSpec <file> Carga la especificacion CSP del sistema."
help[1]="loadImp <file> Carga la implementacion de los predicados y acciones del sistema."
help[2]="compile Realiza la compilacion del interprete junto con la especificacion e implementacion ya cargados. Con -d se compila en el modo debug"
help[3]="run Comienza la ejecucion de la especificacion y la implementacion previamente compilados."
help[4]="quit Cierra el programa."
help[5]="help Muestra un texto de ayuda con informacion del programa."
spec=""
imp=""
NO_EXIT=true
COMPILED=false


echo -e $welcome
while $NO_EXIT ; do
    echo -n $prompt
    read input
    cmd=`echo $input | awk -F ' ' '{print $1}'`
    case $cmd in
        "loadSpec")
            spec=`echo $input | awk -F ' ' '{print $2}'` ;;
        "loadImp")
            imp=`echo $input | awk -F ' ' '{print $2}'` ;;
        "compile")
            if [ "$spec" != ""  ]
            then 
                if [ "$imp" != "" ]
                then
                    flag=`echo $input | awk -F ' ' '{print $2}'`
                    if [ "$flag" = "-d" ]
                    then
                        ghc -cpp -DDEBUG Main.hs $imp -threaded --make -o csp
                    else
                        ghc -cpp Main.hs $imp -threaded --make -o csp
                    fi
                    COMPILED=true
                else
                    echo "Implementacion no cargada."
                fi
            else
                echo "Especificacion no cargada."
            fi ;;
        "run")
            if [ "$spec" != "" ]
            then 
                if [ "$imp" != "" ]
                then
                    if $COMPILED
                    then
                        xterm -e ./csp $spec $imp
                        rm *.dyn*
                        rm *.o
                        rm *.hi
                    else
                        echo "Programa no compilado todavia."
                    fi
                else
                    echo "Implementacion no cargada."
                fi
            else
                echo "Especificacion no cargada."
            fi ;;
        "help")
            for i in `seq 0 5`; do
                echo ${help[$i]}
            done ;;
        "quit")
            NO_EXIT=false ;;
        *)
            echo "no existe el comando, ingrese help para mas informacion"
    esac
done
