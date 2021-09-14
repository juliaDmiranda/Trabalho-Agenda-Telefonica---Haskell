-- Tipo Contato escrito na forma de registro
data Contato = Contato { 
                            nome :: [Char] -- nome do contato
                            ,telefone :: Int -- telefone do contato
                            , endereco :: [Char] -- endereço do contato
                            , relacao :: [Char] -- relação com o contato

                        }

addContato :: [Contato] -> Contato -> [Contato]
addContato [] novoContato = [novoContato]
addContato agendaAntiga novoContato = [novoContato] ++ agendaAntiga  

-- Função toString 
toString :: Contato -> [Char]
toString contato = "Nome: " ++ nome contato 
                                    ++ ", Telefone: " ++ show (telefone contato) 
                                    ++ ", Endereco: " ++ endereco contato 
                                    ++ ", Relacao: " ++ relacao contato 
                                    ++  "///////"                    
-- Função auxiliar de mostrar agenda
mostraAgenda_aux :: [Contato] -> [Char]
mostraAgenda_aux [] = "."
mostraAgenda_aux (contato:agenda) = toString contato ++ mostraAgenda_aux agenda 

-- Função para mostrar agenda
mostraAgenda :: [Contato] -> [Char]
mostraAgenda agenda 
                    | null agenda = "Agenda vazia"
                    | otherwise = mostraAgenda_aux agenda

ehDiferente :: [Char] -> [Char] -> Bool
ehDiferente nomeDoContato nome = nomeDoContato /= nome

{- 
achaContato :: [Char] -> [Contato] -> Contato
achaContato nome agenda 
                        | -}
{- 
remover :: [Contato] -> [Char] -> Contato -> [Contato]
remover agenda nome = filter (ehDiferente nome1 nome) agenda    
                        where 
                           nome1  = (Contato nome _ _ _) -}



contato1 = Contato "Fulano" 99999999 "Rua A" "UFF"
contato2 = Contato "Ciclano" 88888888 "Rua B" "Cederj"
contato3 = Contato "Beltrano" 88889999 "Rua C" "Infância"

agenda = [contato1, contato2, contato3]