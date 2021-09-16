import Data.Char

-- Tipo Contato
data Contato = Contato { 
                            nome :: [Char] -- nome do contato
                            ,telefone :: Int -- telefone do contato
                            , endereco :: [Char] -- endereço do contato
                            , relacao :: [Char] -- relação com o contato

                        }deriving(Show)

-- Função para verificar se todos os caracteres de str1 estão dispostos consecutivamente na str2
estaContido_aux :: [Char] -> [Char] -> [Char]
estaContido_aux [] _ = []
estaContido_aux str1 str2
                        |(toLower( head str1) == toLower (head str2)) = [head str1] ++ (estaContido_aux (tail str1) (tail str2))
                        |otherwise = []

-- Função para verificar se a string str1 está contida na str2
estaContido :: [Char] -> [Char] -> Bool
estaContido str1 [] = False
estaContido str1 str2 = if (toLower( head str1) == toLower (head str2)) && (length (tail str1) == length (estaContido_aux (tail str1) (tail str2)))
                            then True
                        else estaContido str1 (tail str2) 

-- Função de busca do contato
buscar :: [Char] -> [Contato] -> Maybe Contato
buscar _ [] = Nothing
buscar nomeDoContato (contato:agenda)
                                    |estaContido nomeDoContato (nome contato) = Just contato 
                                    |otherwise = buscar nomeDoContato agenda


-- Função para alterar informações de um contato
alterar :: Contato -> [Contato] -> [Contato]
alterar contatoAlterado [] = []
alterar contatoAlterado (contato : agenda)
                                    |((nome contato) == (nome contatoAlterado)) = [contatoAlterado] ++ (alterar contato agenda)
                                    |otherwise = [contato] ++ (alterar contatoAlterado agenda)

-- Função para inserir novo contato na agenda
-- Caso nome já exista na agenda deverá haver uma alteração no contato correspondente 
inserir :: [Contato] -> Contato -> [Contato]
inserir agendaAntiga novoContato
                        | null agendaAntiga = [novoContato]
                        | not (null [contato | contato <- agendaAntiga, (nome contato) == (nome novoContato)]) = alterar novoContato agendaAntiga
                        | otherwise = agendaAntiga ++ [novoContato]

-- Função auxiliar de mostrar agenda
mostrar_aux :: [Contato] -> IO()
mostrar_aux [] = putStrLn("==============================================================================")
mostrar_aux (contato:agenda) = do
                                putStrLn ("Nome: " ++ nome contato 
                                          ++ ", Telefone: " ++ show (telefone contato) 
                                          ++ ", Endereco: " ++ endereco contato  
                                          ++ ", Relacao: " ++ relacao contato)
                                mostrar_aux agenda 
-- Função de mostrar agenda
mostrar :: [Contato] -> IO()
mostrar agenda 
                    | null agenda = putStrLn ("Agenda vazia")
                    | otherwise = do
                                    putStrLn("==============================================================================")
                                    mostrar_aux agenda

-- Função para remover elemento de lista
remover :: [Contato] -> [Char] -> [Contato]
remover agenda nome2 = [contato | contato <- agenda, (nome contato) /= nome2]


-- Operações pedidas no exercício
main :: IO() 
main =  do
                putStrLn("Insere Fulano na agenda vazia\n")
                let a1 = inserir [] (Contato "Fulano" 99999999 "Rua A" "UFF")  
                putStrLn("\nInsere Ciclano na agenda\n")
                let a2 = inserir a1 (Contato "Ciclano" 88888888 "Rua B" "Cederj")
                putStrLn("\nInsere Beltrano na agenda\n")
                let agendaInicial = inserir a2 (Contato "Beltrano" 88889999 "Rua C" "Infância")
                putStrLn("\nInsere Fulano(novamente)\n")
                let agendaInsereFulano = inserir agendaInicial (Contato "Fulano" 77777777 "Rua D" "Churrasco do Ciclano")
                putStrLn("\nRemove Ciclano\n")
                let agendaRemoveCiclano = remover agendaInsereFulano "Ciclano"
                putStrLn("\nMostra Agenda\n")
                mostrar agendaRemoveCiclano
