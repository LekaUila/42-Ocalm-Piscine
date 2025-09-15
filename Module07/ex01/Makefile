# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/09/01 15:06:49 by lflandri          #+#    #+#              #
#    Updated: 2025/09/15 16:34:25 by Leka Uïla        ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

			
SRC		= 	atom.ml \
			aluminium.ml \
			carbon.ml \
			copper.ml \
			hydrogen.ml \
			nickel.ml \
			nitrogen.ml \
			oxygen.ml \
			molecule.ml \
			water.ml \
			carbon_dioxide.ml \
			carbon_monoxide.ml \
			trinitroluene.ml \
			dioxygen.ml \
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
			nitrogen.cmi \
			nitrogen.cmx \
			nitrogen.o \
			oxygen.cmi \
			oxygen.cmx \
			oxygen.o \
			molecule.cmi \
			molecule.cmx \
			molecule.o \
			water.cmi \
			water.cmx \
			water.o \
			carbon_dioxide.cmi \
			carbon_dioxide.cmx \
			carbon_dioxide.o \
			carbon_monoxide.cmi \
			carbon_monoxide.cmx \
			carbon_monoxide.o \
			trinitroluene.cmi \
			trinitroluene.cmx \
			trinitroluene.o \
			dioxygen.cmi \
			dioxygen.cmx \
			dioxygen.o \




NAME 		= ex01


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
BLACKPURPLE	=\\e[1;35m
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
			@echo -e "${BLACKPURPLE}=============== CRÉATION ==============${WHITE}" 
			ocamlopt ${SRC} -o ${NAME}
			@echo -e "${BLACKPURPLE}============= FIN CRÉATION ============${NEUTRE}" 

run	:		all
			./${NAME}


clean :
			@echo -e -n "${BLACKRED}"
			rm -f ${TODELETE}
			@echo -e -n "${NEUTRE}"

fclean :	clean
			@echo -e -n "${BLACKRED}"
			rm -f ${NAME}
			@echo -e -n "${NEUTRE}"


re :		fclean all



.PHONY : all clean fclean re bonus
