# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/12/01 13:17:10 by lflandri          #+#    #+#              #
#    Updated: 2025/12/10 15:55:19 by Leka Uïla        ###   ########.fr        #
#                                                                              #
# **************************************************************************** #


SRC = 		main.ml 

NAME = ex03

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


