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