-- Tipo Contato escrito na forma de registro
data Contato = Contato { 
                            nome :: [Char] -- nome do contato
                            ,telefone :: Int -- telefone do contato
                            , endereco :: [Char] -- endereço do contato
                            , relacao :: [Char] -- relação com o contato

                        } deriving(Show, Eq)

-- Funções que retornam as informações do contado separadamente
getnome :: Contato -> [Char]
getnome (Contato nome _ _ _) = nome

gettelefone :: Contato -> Int
gettelefone (Contato _ telefone _ _) = telefone

getendereco :: Contato -> [Char]
getendereco (Contato _ _ endereco _) = endereco

getrelacao :: Contato -> [Char]
getrelacao (Contato _ _ _ relacao) = relacao

addContato :: [Contato] -> Contato -> [Contato]
addContato [] novoContato = [novoContato]
addContato agendaAntiga novoContato = [novoContato] ++ agendaAntiga  

-- Função toString 
toString :: Contato -> [Char]
toString contato = "Nome: " ++ getnome contato 
                                    ++ ", Telefone: " ++ show (gettelefone contato) 
                                    ++ ", Endereco: " ++ getendereco contato 
                                    ++ ", Relacao: " ++ getrelacao contato 

-- Função auxiliar de mostrar agenda
mostraAgenda :: [Contato] -> [Char]
mostraAgenda_aux [] = "."
mostraAgenda_aux (contato:agenda) = toString contato ++ mostraAgenda_aux agenda 

-- Função para mostra agenda
mostraAgenda :: [Contato] -> [Char]
mostraAgenda agenda 
                    | null agenda = "Agenda vazia"
                    | otherwise = mostraAgenda_aux agenda



contato1 = Contato "Fulano" 99999999 "Rua A" "UFF"
contato2 = Contato "Ciclano" 88888888 "Rua B" "Cederj"
contato3 = Contato "Beltrano" 88889999 "Rua C" "Infância"

agenda = [contato1, contato2, contato3]