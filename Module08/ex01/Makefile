# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/12/01 13:17:10 by lflandri          #+#    #+#              #
#    Updated: 2025/12/04 13:20:03 by Leka Uïla        ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


SRC = 		app.ml \
			main.ml 

NAME = ex01

OBJS = $(SRC:.ml=.cmx) 
TODELETE = $(OBJS) $(SRC:.ml=.o)  $(SRC:.ml=.cmi)

all: $(NAME)

%.cmx: %.ml
	ocamlopt -c $< ${<:.ml=.cmx}

$(NAME): $(OBJS) $(CMIFILE)
	ocamlopt -o $(NAME)  $(OBJS) 

clean:
	rm $(TODELETE)


fclean: clean
	rm -f $(NAME)

run : all
	./$(NAME)

re : fclean all


