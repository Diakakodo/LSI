CC = ocamlc
CFLAGS =
LDFLAGS = set_lsi.ml set.ml
OBJECTS = set_lsi.cmo set_lsi.cmi set.cmo set.cmi
executable = set

all : executable

executable : $(LDFLAGS) 
	$(CC) -o $(executable) $(LDFLAGS) 

set_lsi.cmo : set_lsi.cmi set_lsi.ml
set_lsi.cmi : set_lsi.ml
set.cmo : set_lsi.cmo set.ml
set.cmi : set_lsi.cmo set.ml


clean :
# sous Linux
	rm -f $(executable) $(executable).cmo $(executable).cmi

#ocamlc -o calc gdnb.ml calc.ml
