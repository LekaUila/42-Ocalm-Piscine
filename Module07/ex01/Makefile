# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/12/01 13:17:10 by lflandri          #+#    #+#              #
#    Updated: 2025/12/01 14:12:35 by lflandri         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


SRC = atom.ml \
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

NAME = ex01

OBJS = $(SRC:.ml=.cmx)

TODELETE = $(OBJS) $(SRC:.ml=.o)  $(SRC:.ml=.cmi)


all: $(NAME)

%.cmx: %.ml
	ocamlopt -c $< ${<:.ml=.cmx}	


$(NAME): $(OBJS)
	ocamlopt -o $(NAME)  $(OBJS)

clean:
	rm $(TODELETE)


fclean: clean
	rm -f $(NAME)

run : all
	./$(NAME)

re : fclean all


