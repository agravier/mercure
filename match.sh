echo "$# $1 $2"

if [ $# -ne 2 ] 
then
    exit 1
fi

java -jar tools/PlayGame.jar maps/map7.txt 3000 1000 log.txt "java -jar $1" "java -jar $2" | java -jar tools/ShowGame.jar
