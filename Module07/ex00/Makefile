# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/12/01 13:17:10 by lflandri          #+#    #+#              #
#    Updated: 2025/12/01 14:11:13 by lflandri         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


SRC = atom.ml \
			aluminium.ml \
			carbon.ml \
			copper.ml \
			hydrogen.ml \
			nickel.ml \
			oxygen.ml \
			main.ml 

NAME = ex00

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


