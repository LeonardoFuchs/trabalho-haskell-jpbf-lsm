-- trabalho Haskell
-- No final do script, exemplo de recursão
cpalavras = ['F','L','O','R','E','S','T','A','R','S',
			'K','J','G','D','F','T','J','J','U','V',
			'C','A','P','E','L','A','P','I','S','C',
			'D','T','C','A','D','W','Y','E','R','T',
			'F','D','C','C','S','A','L','U','S','E',
			'Q','L','K','C','A','U','E','A','I','O',
			'A','A','S','A','R','P','R','D','K','A',
			'N','E','N','E','F','T','E','B','A','U',
			'J','K','D','D','S','H','W','L','F','L',
			'I','O','T','U','G','J','J','V','A','A']
			
			
cpalavras1 = ['S', 'A', 'B', 'O', 'N', 'E', 'T', 'E', 'J', 'P',
			'A', 'M', 'H', 'G', 'J', 'L', 'P', 'J', 'A', 'I',
			'L', 'I', 'M', 'A', 'O', 'L', 'V', 'W', 'C', 'C',
			'S', 'A', 'B', 'O', 'N', 'E', 'T', 'E', 'A', 'L',
			'I', 'Q', 'F', 'C', 'T', 'M', 'U', 'H', 'T', 'E',
			'C', 'W', 'Z', 'S', 'E', 'Y', 'K', 'J', 'I', 'S',
			'H', 'G', 'D', 'N', 'X', 'C', 'Q', 'J', 'A', 'L',
			'A', 'R', 'U', 'I', 'N', 'U', 'S', 'A', 'C', 'O',
			'K', 'F', 'P', 'O', 'W', 'Q', 'T', 'K', 'A', 'D',
			'M', 'A', 'N', 'T', 'E', 'I', 'G', 'A', 'J', 'D']
			
lcustos = [('A',1),('B',10),('C',18),('D',38),('E',70),('F',10),('G',43),('H',15),('I',54),('J',10),
			('K',44),('L',81),('M',19),('N',39),('O',54),('P',91),('Q',72),('R',30),('S',84),('T',75),('U',22),
			('V',47),('X',66),('Y',59),('Z',32)]
			
palavra = ["AULA", "ALEGRIA", "CAPELA", "FLORESTA", "LAPIS", "CAU"]

palavras1 = ["SABONETE", "SALSICHA", "MANTEIGA", "LIMAO", "PICLES", "JACA"]

lcustos::[(Char, Int)] 			-- Define a lista lcustos como uma lista de tuplas onde o 1º elemento da tupla é um "Char" e o 2º é um "Int" pra não dar erro de tipo nas funções abaixo

-- Jordão teaching
matrizlinha xs x = if null xs			-- separa o caça-palavras em listas de tamanho n (que seria a dimensão do caça palavras) na forma de linhas
					then []				-- Recebe de entrada a matriz do caça-palavras e o tamanho de cada linha do mesmo
					else [take x xs] ++ matrizlinha (drop x xs) x
matrizcoluna xs x = if (length xs ) == (x^2-x)				-- separa o caça-palavras em listas de tamanho n (que seria a dimensão do caça palavras) na forma de colunas
					then []								-- Recebe de entrada a matriz do caça-palavras e o tamanho de cada coluna do mesmo
					else [[ xs!!y | y<-[x*0,x*1..(x*(x-1))]]] ++ matrizcoluna (tail xs) x
funcao12 xs ys = if null ys 			-- Verifica se uma string pertence à uma lista de strings. (Uma aplicação da função sublista em várias strings simultaneamente)
				then False
				else sublista xs (head ys) || funcao12 xs (tail ys)
capalavra xs ys = [ x | x<-xs, funcao12 x ys]
-- Procura as palavras de uma lista de palavras no caça-palavras indicado. Retorna uma lista com as palavras encontradas. xs: lista de palavras, ys: matriz do caça-palavras

palavrasexist xs ys n = capalavra xs w ++ capalavra xs v ++ capalavra palavra (revertestrin w) ++ capalavra palavra (revertestrin v) 
					where
					w = matrizlinha ys n
					v = matrizcoluna ys n
-- A função palavrasexist recebe de entrada a lista de palavras e o tamanho de cada linha/coluna e analisa quais palavras são encontradas no caça-palavras (esquerda>direita, cima>baixo e vice-versa)
-- ys é o caça-palavras
prefixo xs ys = xs == take n ys				-- Verifica se os elementos de uma lista é o prefixo de uma outra lista. (ex.: "FLOR" "FLOR~ESTA"
					where 					-- Entrada xs ys são listas.
					n = length xs
sublista xs ys = if null ys 				-- Verifica se os elementos de uma lista - de modo ordenado - pertence à outra lista. (ex.: "gato" "O gato esta aqui")
					then False				-- Admite como entrada em: xs e ys como listas 
						else prefixo xs ys || sublista xs (tail ys)
revertestrin xs = [reverse x | x<-xs] 		-- devolve a lista de strings na ordem inversa de cada uma. (ex.: [AULA, BACANA] -> [ALUA, ANACAB]

-- 1ª questão
ocorre x xs = length [ y | y <- xs, x == y] -- x é a 'letra' a ser analisada e xs é a "palavra" onde a letra aparece
custopal1 xs ys = if null xs				-- xs é a lista de custos (lcustos)  ys é a "string" da palavra
					then 0
					else (ocorre (fst (head xs)) ys)*(snd (head xs)) + custopal1 (tail xs) ys
funcao xs ys = [custopal1 xs y | y <- ys]		-- xs é a lista de custos (lcustos) e ys é a lista de palavras (palavra).
											--Retorna a lista de custos.
analise1 xs ys = zip ys x  					-- ys é a lista de palavras e xs é a lista de custos
				where
				x = funcao xs ys 			--x é a lista de custos de cada palavra. 
											-- analise1 retorna uma lista de tuplas onde o 1º elemento de cada tupla é a PALAVRA e o segundo, seu CUSTO.
palavramenorc xs ys  = [ x | x <-analise1 xs ys, (snd x) == minimum p] -- A função palavramenorc recebe xs: lista de custos; ys: lista de palavras
				where						-- analisa as tuplas de (palavra, custo) e retorna com a lista de menor custo.
				p = funcao xs ys			--  p é a lista de custos (ex.: [1, 10, 105, 1231])
				
ocorrencia1 xs ys n zs = palavramenorc zs (palavrasexist xs ys n) 
--A função admite de entrada a lista de palavras, o caça-palavras, a dimensão da matriz e a lista de custos e aplica a função -palavramenorc- (que aplica a função - palavrasexist - com as entradas dadas! 
ocorrenciamc xs ys n zs = if null (palavramenorc zs (palavrasexist xs ys n))
			  then error "Nenhuma palavra encontrada na matriz!"
			  else palavramenorc zs (palavrasexist xs ys n)
-- off



-- 2ª questão
numocorrencia x zs ys n = length [ y | y <- xs, x==y]	-- a função recebe em x a palavra a ser analisada (entre aspas) e a lista de palavras que estão sendo procuradas no caça-palavras e a dimensão
					where 							-- e retorna quantas vezes essa palavra aparece no caça-palavras
					xs = palavrasexist ys zs n
					
tuplaocorr x xs ys n =if z == 0 then   --retorna o número de ocorrência de cada palavra na matriz, se o número de ocorrências for igual a 0, retorna lista vazia
					 []
					else [(x,z)] --[(x, z)]
					where 
					z = numocorrencia x ys xs n
					
listatuplaocorr xs ys n = concat [tuplaocorr x xs ys n| x<-xs] -- A função recebe de entrada a lista de palavra em xs E em ys e a dimensão do caça-palavras e retorna uma lista de duplas (PALAVRA, nº de ocorrencias)

nocorrencias xs ys n = (tuplaordemalfabetica)  -- concat é usado para concatenar as listas vazias criadas pelas palavras que ocorrem 0 vezes com as que não ocorrem, evitando assim uma lista com listas vazias em seu meio.
						where tuplaordemalfabetica =  (quicksort (listatuplaocorr xs ys n))
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) --usado para deixar a lista de palavras encontradas em ordem alfabética
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

tuplacusto xs zs n ys = zip x a   --cria uma lista de tuplas com a palavra e seu respectivo custo
					where  
							a = palavrasexist xs zs n
							x = funcao ys (palavrasexist xs zs n)

palavrasmaiscaras1 xs zs n ys = reverse (quicksort (custpal))     --o quicksort ordena em ordem crescente, queremos do mais caro pro menos caro, então utilizo a função reverse
								where custpal = tuplacusto xs zs n ys

invertTup list = [(x,y) | (y,x)<- list]

palavrasmaiscaras xs zs n ys = invertTup (palavrasmaiscaras1 xs zs n ys)

--palavrasmaiscaras xs ys lcustos = 
							
-- FIM!
-- >When I wrote this, only God and I understood what I was doing
-- >Now, God only knows



-- Paiva's thing
posicaoLet b xs = (length (takeWhile (/= b) letras))
    where 
    letras = [ x | (x,y) <- xs]

custoPal bs xs = if null bs
    then 0
    else (custos!!(posicaoLet b xs)) + (custoPal (tail bs) xs)
    where
    b = (head bs)
    letras = [ x | (x,y) <- xs]
    custos = [ y | (x,y) <- xs]

-- no começo do script, exemplo de recursão
porquito = putStr "O PORCO DA SALVAÇÃO CHEGOU\n                          _\n  _._ _..._ .-',     _.._(`))\n '-. `     '  (-._.-'    ',)\n    )         )            '.\n   ( _    _    |             )\n  |  a    a    )              |\n  (   .-.                     ;  \n   '-('' ).-'       ,'       ;\n      '-;           |      .'\n         )           (    )\n         | 7  .__  _.-(   )\n         | |  |  ``(  (`  )\n        (,_|  |   (,_(   )\n           (,_(      '`-'\n"
