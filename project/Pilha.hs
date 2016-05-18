module Pilha (Pilha
			  ,esvazia
			  ,empilha
			  ,desempilha
			  ,retornaTopo
			  ,converteListaEmPilha) where

	data Pilha p = PilhaVazia | Pilha [p]
		deriving (Eq, Show)

	esvazia :: Pilha p -> Pilha p
	esvazia _ = PilhaVazia

	empilha :: p -> Pilha p -> Pilha p
	empilha valor (PilhaVazia) = Pilha[valor]
	empilha valor (Pilha []) = Pilha[valor]
	empilha valor (Pilha complemento) = Pilha(valor:complemento)

	desempilha :: Pilha p -> Pilha p
	desempilha (Pilha(x:y)) = Pilha y

	retornaTopo :: Pilha p -> p
	retornaTopo (Pilha(x:y)) = x

	converteListaEmPilha :: [p] -> Pilha p
	converteListaEmPilha lista = Pilha lista