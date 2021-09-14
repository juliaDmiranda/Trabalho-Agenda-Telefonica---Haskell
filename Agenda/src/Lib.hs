-- Tipo Contato escrito na forma de registro
data Contato = Contato { 
                            nome :: [Char] -- nome do contato
                            ,telefone :: Int -- telefone do contato
                            , endereco :: [Char] -- endereço do contato
                            , relacao :: [Char] -- relação com o contato

                        }

-- Função de busca do contato (ainda falta ajeitar para procurar só parte do nome)
procurar :: [Char] -> [Contato] -> Bool
procurar nomeDoContato [] = False
procurar nomeDoContato ((Contato nome _ _ _) : agenda) = if (nome == nomeDoContato)
                                    then True
                                    else procurar nomeDoContato agenda

-- Função para alterar informações de um contato
alterar :: Contato -> [Contato] -> [Contato]
alterar contato [] = []
alterar contato ((Contato nome1 telefone1 endereco1 relacao1) : agenda) = if (nome1 == (nome contato))
                                    then [contato] ++ (alterar contato agenda)
                                    else [cabeca] ++ (alterar contato agenda)
                                        where cabeca = (Contato nome1 telefone1 endereco1 relacao1)
-- Função auxiliar de mostrar agenda
mostraAgenda_aux :: [Contato] -> [Char]
mostraAgenda_aux [] = "."
mostraAgenda_aux (contato:agenda) = toString contato ++ mostraAgenda_aux agenda 

-- Função para mostrar agenda
mostraAgenda :: [Contato] -> [Char]
mostraAgenda agenda 
                    | null agenda = "Agenda vazia"
                    | otherwise = mostraAgenda_aux agenda

-- Função para inserir novo contato na agenda
-- Caso nome já exista na agenda deverá haver uma alteração no contato correspondente 
inserir :: [Contato] -> Contato -> [Contato]
inserir agendaAntiga novoContato
                        | null agendaAntiga = [novoContato]
                        | procurar (nome novoContato) agendaAntiga = alterar novoContato agendaAntiga
                        | otherwise = agendaAntiga ++ [novoContato]

-- Função auxiliar de mostrar agenda
mostrar_aux :: [Contato] -> IO()
mostrar_aux [] = putStrLn("==============================================================================")
mostrar_aux (contato:agenda) = do
                                    putStrLn ("Nome: " ++ nome contato ++ ", Telefone: " ++ show (telefone contato) ++ ", Endereco: " ++ endereco contato ++ ", Relacao: " ++ relacao contato ++  "")
                                    mostrar_aux agenda 

-- Função para mostrar agenda
mostrar :: [Contato] -> IO()
mostrar agenda 
                    | null agenda = putStrLn ("Agenda vazia")
                    | otherwise = do
                                    putStrLn("==============================================================================")
                                    mostrar_aux agenda


contato1 = Contato "Fulano" 99999999 "Rua A" "UFF"
contato2 = Contato "Ciclano" 88888888 "Rua B" "Cederj"
contato3 = Contato "Beltrano" 88889999 "Rua C" "Infância"

agenda = [contato1, contato2, contato3]