;ultima_funcao______varGlobal
;remove_letras_string
;remove_letras_string_____STRING1
;remove_numeros_string
;remove_numeros_string_____STRING1
;CriarLink
;criarlink
;CriarLink_____ent_link_codigo
;criarlink_____ent_link_codigo
;GetId
;GetId_____entd_nomeLink
;getid
;getid_____entd_nomeLink
;faz_janela
;faz_janela_____COORD_TIPO_PROPRIEDADE_TAMANHO
;desenha_linha
;desenha_linha_____p1_p2_layer_cor
;desenha_circulo
;desenha_circulo_____ponto_raio_layer_cor
;quantidade_elementos_array
;quantidade_elementos_array_____array1
;pega_propriedade_objeto
;pega_propriedade_objeto_____obj_prop
;gerar_lista_elementos
;gerar_lista_elementos_____layer_tipo_elementos
;retorna_attrib
;retorna_attrib_____BLOCO_numATTRIB
;gera_layer
;gera_layer_____nomeLayer_Cor
;arredonda_coordenada
;arredonda_coordenada_____coord_casas
;exibir_porcentagem
;exibir_porcentagem_____Qtd
;verxdata
;verxdata_____semParametros
;ViewExtents
;ViewExtents_____semParametros
;viewextents
;viewextents_____semParametros
;distancia_ponto_reta
;distancia_ponto_reta_____p1,p2,ponto_insercao

;cuidado ao usar a variavel contador





(defun c:helpbf()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	(princ "\n===========MENU===============")
	(princ "\nSistemas...")
	(princ "\n[1] Sistema de aeracao fixo 1")
	(princ "\n[2] Sistema de aeracao fixo 2")
	(princ "\n[3.1] Sistema de aeracao fixo 3 ESTRUTURA")
	(princ "\n[3.2] Sistema de aeracao fixo 3")
	(princ "\n[4] Sistema de air grid 1")
	(princ "\nEntradas de ar...")
	(princ "\n[ea1] Entrada de ar aerecao fixa 1")
	(princ "\n[ea2] Entrada de ar aerecao fixa 2")
	
	(princ "\nEdição...")
	(princ "\n[sa1] Insere SA 1")
	(princ "\n[sg1] Insere SG 1")
	(princ "\n[sg12] Insere SG 1.2 (TRIM)")
	(princ "\n[sa2] Insere SA 2")
	(princ "\n[sg2] Insere SG 2")
	(princ "\n[sa3] Insere SA 3")
	(princ "\n[sg3] Insere SG 3")
	
	(princ "\nLista de materiais...")
	(princ "\n[mgr1] air grid lista 1")
	
	
	(princ "\nOutros...")
	(princ "\n[tl] Desenha tanque, ou lagoa")
	
	
	(princ)
)







