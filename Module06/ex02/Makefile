# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/12/01 13:17:10 by lflandri          #+#    #+#              #
#    Updated: 2025/12/01 14:10:58 by lflandri         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


SRC = people.ml \
			doctor.ml \
			dalek.ml \
			main.ml 

NAME = ex02

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


