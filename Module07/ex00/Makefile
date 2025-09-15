# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/09/01 15:06:49 by lflandri          #+#    #+#              #
#    Updated: 2025/09/15 13:52:49 by Leka Uïla        ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

			
SRC		= 	atom.ml \
			aluminium.ml \
			carbon.ml \
			copper.ml \
			hydrogen.ml \
			nickel.ml \
			oxygen.ml \
			main.ml 
		  
TODELETE = 	main.cmi \
			main.cmx \
			main.o \
			atom.cmi \
			atom.cmx \
			atom.o \
			aluminium.cmi \
			aluminium.cmx \
			aluminium.o \
			carbon.cmi \
			carbon.cmx \
			carbon.o \
			copper.cmi \
			copper.cmx \
			copper.o \
			hydrogen.cmi \
			hydrogen.cmx \
			hydrogen.o \
			nickel.cmi \
			nickel.cmx \
			nickel.o \
			oxygen.cmi \
			oxygen.cmx \
			oxygen.o \



NAME 		= ex00


MAKEFLAGS	= --no-print-directory

#couleur
BLACK		=\\e[0;30m
GREY		=\\e[1;30m
BLACKRED	=\\e[0;31m
PINK		=\\e[1;31m
BLACKGREEN	=\\e[0;32m
WHITEGREEN	=\\e[1;32m
ORANGE		=\\e[0;33m
YELLOW		=\\e[1;33m
BLACKBLUE	=\\e[0;34m
WHITEBLUE	=\\e[1;34m
BLACKPURPLE	=\\e[0;35m
WHITEPURPLE	=\\e[;1;35m
BLACKCYAN	=\\e[0;36m
WHITECYAN	=\\e[1;36m
WHITEGREY	=\\e[0;37m
WHITE		=\\e[1;37m

NEUTRE		= \\e[0;m

#effect
BLINK		=\\e[5m

RESET		=\\e[25m

all :		${NAME}
# 			@echo "${WHITEGREEN}PROGRAMME \"${BLINK}${NAME}${RESET}\" PRET POUR EXECUTION !${NEUTRE}"

${NAME}: 
			@echo "${BLACKPURPLE}=============== CRÉATION ==============${WHITE}" 
			ocamlopt ${SRC} -o ${NAME}
			@echo "${BLACKPURPLE}============= FIN CRÉATION ============${NEUTRE}" 

run	:		all
			./${NAME}


clean :
			@echo -n "${BLACKRED}"
			rm -f ${TODELETE}
			@echo -n "${NEUTRE}"

fclean :	clean
			@echo -n "${BLACKRED}"
			rm -f ${NAME}
			@echo -n "${NEUTRE}"


re :		fclean all



.PHONY : all clean fclean re bonus
