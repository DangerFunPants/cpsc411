for file in $1/*.mn
do
    printf "Tokens in file: $file\n"
    ./lexer "$file" 
    printf "\n"
done