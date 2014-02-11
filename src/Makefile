#

ERLC		=	erlc
ERL			=	erl -boot start_clean
OBJ_DIR		=	../ebin
INCLUDE_DIR	=	../include

SRCS		=	$(wildcard *.erl)
INCLUDES	=	$(wildcard $(INCLUDE_DIR)/*.hrl)
OBJS		=	$(SRCS:%.erl=$(OBJ_DIR)/%.beam)

EFLAGS		=	-I $(INCLUDE_DIR) -o $(OBJ_DIR) -W +debug_info -smp # -Wall +native -v

all: $(OBJS)

$(OBJ_DIR)/%.beam: %.erl $(INCLUDES)
	erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJ_DIR)/*.beam $(OBJ_DIR)/erl_crash.dump erl_crash.dump
