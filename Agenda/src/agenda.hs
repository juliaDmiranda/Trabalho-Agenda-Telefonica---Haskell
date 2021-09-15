import Data.Char

-- Tipo Contato escrito na forma de registro
data Contato = Contato { 
                            nome :: [Char] -- nome do contato
                            ,telefone :: Int -- telefone do contato
                            , endereco :: [Char] -- endereço do contato
                            , relacao :: [Char] -- relação com o contato

                        }deriving(Show)

-- Função para verificar se todos os caracteres de str1 estão dispostos consecutivamente na str2
estaContido_aux :: [Char] -> [Char] -> [Char]
estaContido_aux [] _ = []
estaContido_aux str1 str2 = if (toLower( head str1) == toLower (head str2))
                                    then [head str1] ++ (estaContido_aux (tail str1) (tail str2))
                                    else []
-- Função para verificar se a string str1 está contida na str2
estaContido :: [Char] -> [Char] -> Bool
estaContido str1 [] = False
estaContido str1 str2 = if (toLower( head str1) == toLower (head str2)) && (length (tail str1) == length (estaContido_aux (tail str1) (tail str2)))
                                    then 
                                        True
                                else estaContido str1 (tail str2) 

-- Função de busca do contato
buscar :: [Char] -> [Contato] -> [Contato]
buscar _ [] = []
buscar nomeDoContato (contato:agenda) = if estaContido nomeDoContato (nome contato)
                                    then [contato] ++ buscar nomeDoContato []
                                    else buscar nomeDoContato agenda


-- Função para alterar informações de um contato
alterar :: Contato -> [Contato] -> [Contato]
alterar contatoAlterado [] = []
alterar contatoAlterado (contato : agenda) = if ((nome contato) == (nome contatoAlterado))
                                    then [contatoAlterado] ++ (alterar contato agenda)
                                    else [contato] ++ (alterar contatoAlterado agenda)

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
                                               ++ ", Telefone: "
                                               ++ show (telefone contato) 
                                               ++ ", Endereco: " ++ endereco contato 
                                               ++ ", Relacao: " 
                                               ++ relacao contato)
                                    mostrar_aux agenda 

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
trabalho :: IO() 
trabalho = 
            do
                putStrLn("Inserindo os contatos na agenda vazia")
                putStrLn("")
                let agendaInicial = [(Contato "Fulano" 99999999 "Rua A" "UFF"), (Contato "Ciclano" 88888888 "Rua B" "Cederj"), (Contato "Beltrano" 88889999 "Rua C" "Infância")]
                mostrar agendaInicial
                putStrLn("")
                putStrLn("Altera Fulano")
                putStrLn("")
                let agendaInsereFulano = inserir agendaInicial (Contato "Fulano" 77777777 "Rua D" "Churrasco do Ciclano")
                mostrar agendaInsereFulano
                putStrLn("")
                putStrLn("Remove Ciclano")
                putStrLn("")
                let agendaRemoveCiclano = remover agendaInsereFulano "Ciclano"
                mostrar agendaRemoveCiclano