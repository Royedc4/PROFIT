PROCEDURE BeforeOpenTables
SET DELETED ON
DO open_dat WITH THIS
RETURN

ENDPROC
PROCEDURE Init
SET CENTURY ON
PUBLIC tnro
LOCAL llcontinue, formato_dolar
PUBLIC tnro, ttip_cob1, tnum_doc1, tmont_doc1, tbanco1, tnombre_ban1
PUBLIC ttip_cob2, tnum_doc2, tmont_doc2, tbanco2, tnombre_ban2
PUBLIC ttip_cob3, tnum_doc3, tmont_doc3, tbanco3, tnombre_ban3, timp1, timp2, timp3, texen
SET DATASESSION TO

rtipo_aux = rtipo
abrir_logo()
formato_dolar = .F.
ttip_doc = "VXXX"
*depurador("AD",.t.)
*** VerificaciÃƒÂ³n de que existan las variables pÃƒÂºblicas para IVA, RIF y NIT  (si no existen se asignan)
If TYPE('TL_IVA')='U'
	Public TL_IVA
	TL_IVA='I.V.A.'
Endif
If TYPE('TL_NIT')='U'
	Public TL_NIT
	TL_NIT='N.I.T.'
Endif
If TYPE('TL_RIF')='U'
	Public TL_RIF
	TL_RIF='R.I.F.'
Endif
***
Release lc_serie
Public lc_serie
lc_serie='XXX'
***
Do CASE
	Case rtipo = 0 OR rtipo = 1
		ttabla = "Factura."
		tfrom  = "Factura"
		tsaldo = .T.
		tnro   = "Factura:"
		ttabla_reng = "reng_fac."
		tfrom_reng = "reng_fac"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
		ttip_doc = "VFAC"
		lc_serie='FAC'

	Case rtipo = 2
		ttabla = "Cotiz_c."
		tfrom  = "Cotiz_c"
		tnro   = "CotizaciÃƒÂ³n:"
		ttabla_reng = "reng_cac."
		tfrom_reng = "reng_cac"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
	Case rtipo = 3
		ttabla = "dev_cli."
		tfrom  = "dev_cli"
		tnro   = "Devoluc Cliente:"
		ttabla_reng = "reng_dvc."
		tfrom_reng = "reng_dvc"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
		ttip_doc = "VDVC"
	Case rtipo = 4
		ttabla = "Not_ent."
		tfrom  = "Not_ent"
		ttabla_reng = "reng_NDE."
		tfrom_reng = "reng_NDE"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
		tnro     = "Nota de Entrega:"
		ttip_doc = "VNDE"
		lc_serie='NDE'
	Case rtipo = 5
		ttabla = "Not_dep."
		tfrom  = "Not_dep"
		ttabla_reng = "reng_ndd."
		tfrom_reng = "reng_ndd"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
		tnro   = "Nota de Despacho:"
		ttip_doc = "VNDD"
	Case rtipo = 6
		ttabla = "pedidos."
		tfrom  = "pedidos"
		tnro   = "Pedido:"
		ttabla_reng = "reng_ped."
		tfrom_reng = "reng_ped"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
		ttip_doc = "VPED"
	Case rtipo = 7       && tipo de reporte compras
		ttabla = "Compras."
		tfrom  = "Compras"
		tnro = "Compra:"
		ttabla_reng = "reng_com."
		tfrom_reng = "reng_com"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
		ttip_doc = "CCOM"
	Case rtipo = 8
		ttabla = "Cotiz_p."
		tfrom  = "Cotiz_p"
		tnro   = "CotizaciÃƒÂ³n de Proveedor:"
		ttabla_reng = "reng_cdp."
		tfrom_reng = "reng_cdp"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
	Case rtipo = 9
		ttabla = "dev_pro."
		tfrom  = "dev_pro"
		tnro   = "Devoluc Proveedor:"
		ttabla_reng = "reng_dvp."
		tfrom_reng = "reng_dvp"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
		ttip_doc = "CDVP"
	Case rtipo = 10
		ttabla = "Not_rec."
		tfrom  = "Not_rec"
		tnro   = "Nota de RecepciÃƒÂ³n:"
		ttabla_reng = "reng_ndr."
		tfrom_reng = "reng_ndr"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
		ttip_doc = "CNDR"
	Case rtipo = 11
		ttabla = "Ordenes."
		tfrom  = "Ordenes"
		tnro   = "Orden:"
		ttabla_reng = "reng_ord."
		tfrom_reng = "reng_ord"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
	Case rtipo = 12
		ttabla = "Plavent."
		tfrom  = "Plavent"
		tnro   = "Plantilla de ventas:"
		ttabla_reng = "reng_plv."
		tfrom_reng = "reng_plv"
		ttabla1   = "CLIENTES."
		tfrom1    = "CLIENTES"
	Case rtipo = 13
		ttabla = "Placom."
		tfrom  = "Placom"
		tnro   = "Plantilla de compras:"
		ttabla_reng = "reng_plc."
		tfrom_reng = "reng_plc"
		ttabla1   = "PROV."
		tfrom1    = "PROV"
Endcase


ttabla4 ="kit."
tfrom4  ="kit"
ttabla5 ="reng_kit."
tfrom5  ="reng_kit"

*IF !(rtipo<=6 OR rtipo=12)
*		ttabla3 = 'vendedor.'
*		tfrom3  = 'vendedor'
*		tcampo  = 'co_ven'
*		tcampo2 = 'ven_des'
*ELSE
	ttabla3 = 'transpor.'
	tfrom3  = 'transpor'
	tcampo  = 'co_tran'
	tcampo2 = 'des_tran'
*ENDIF



IF vpar_emp.p_imp_kit
	treport = "VREPAUX"
ELSE
	treport = "VREPORTES"
ENDIF
tdivide = !formato_dolar

ttip_cob1    = SPACE(4)
tnum_doc1    = 0
tmont_doc1   = 0
tbanco1      = SPACE(4)
tnombre_ban1 = SPACE(4)
ttip_cob2    = SPACE(4)
tnum_doc2    = 0
tmont_doc2   = 0
tbanco2      = SPACE(4)
tnombre_ban2 = SPACE(4)
ttip_cob3    = SPACE(4)
tnum_doc3    = 0
tmont_doc3   = 0
tbanco3      = SPACE(4)
tnombre_ban3 = SPACE(4)

IF serversql
	tconnect = SQLCONECTAR()
	IF  mensaje_sql(tconnect,0) <= 0
		RETURN .F.
	ENDIF
ENDIF

*******************************************************************************************************
*****************SELECT PARA CONSEGUIR LA FORMA DE PAGO O COBRO DE LA FACTURA ***************************
IF tdesde(1) = thasta(1) AND (rtipo = 0 OR rtipo =1 OR rtipo = 7)
	IF rtipo = 7
		ttab    = 'compras.'
		tftab   = 'compras'
		treng   = 'reng_pag.'
		tfreng  = 'reng_pag'
		tforma  = 'reng_tcp.'
		tfforma = 'reng_tcp'
	ELSE
		ttab    = 'factura.'
		tftab   = 'factura'
		treng   = 'reng_cob.'
		tfreng  = 'reng_cob'
		tforma  = 'reng_tip.'
		tfforma = 'reng_tip'
	ENDIF

	***Select para cosegir la primera forma de pago en cobros de una Factura*****
	IF serversql
		tca1 = '&tforma.tip_cob, &tforma.num_doc, &tforma.mont_doc, &tforma.banco, &tforma.nombre_ban, &treng.tp_doc_cob'
		tfr1 = '&tftab, &tfreng, &tfforma '
		twh1 = '&ttab.fact_num BETWEEN '+ALLTRIM(STR(tdesde(1)))+' AND '+ALLTRIM(STR(thasta(1)))+' AND &treng.doc_num = &ttab.fact_num AND &treng.cob_num = &tforma.cob_num '
		tresult=sqlexec(tconnect,'select '+tca1+' from '+tfr1+' WHERE '+twh1,'vrepaux3')

	ELSE
		SELECT &tforma.tip_cob, &tforma.num_doc, &tforma.mont_doc,;
			&tforma.banco, &tforma.nombre_ban, &treng.tp_doc_cob;
			FROM bldatos!&tftab, bldatos!&tfreng, bldatos!&tfforma;
			WHERE &ttab.fact_num BETWEEN ?tdesde(1) AND ?thasta(1);
			AND (&treng.doc_num = &ttab.fact_num AND &treng.tp_doc_cob="FACT");
			AND  &treng.cob_num  = &tforma.cob_num;
			INTO CURSOR vrepaux3
	ENDIF

	IF USED('vrepaux3')
		SELECT vrepaux3.*;
			FROM vrepaux3;
			WHERE vrepaux3.tp_doc_cob='FACT';
			INTO CURSOR vrepaux3 nofilter

		DO WHILE !EOF('vrepaux3')
			DO CASE
				CASE RECNO() = 1
					ttip_cob1    = vrepaux3.tip_cob
					tnum_doc1    = vrepaux3.num_doc
					tmont_doc1   = vrepaux3.mont_doc
					tbanco1      = vrepaux3.banco
					tnombre_ban1 = vrepaux3.nombre_ban
				CASE RECNO() = 2
					ttip_cob2    = vrepaux3.tip_cob
					tnum_doc2    = vrepaux3.num_doc
					tmont_doc2   = vrepaux3.mont_doc
					tbanco2      = vrepaux3.banco
					tnombre_ban2 = vrepaux3.nombre_ban
				CASE RECNO() = 3
					ttip_cob3    = vrepaux3.tip_cob
					tnum_doc3    = vrepaux3.num_doc
					tmont_doc3   = vrepaux3.mont_doc
					tbanco3      = vrepaux3.banco
					tnombre_ban3 = vrepaux3.nombre_ban
			ENDCASE
			SKIP 1 IN vrepaux3
		ENDDO
	ENDIF
ENDIF

IF rtipo=0 OR rtipo=1
	tvuelto = ttabla+'vuelto'
ELSE
	tvuelto = IIF(serversql,ttabla+"tot_neto",'.t.')
ENDIF
tresult = 0
IF rtipo <=  6  OR rtipo = 12   && select de ventas
	IF rtipo = 3
		ttipo ='E'
		NOTA = ttabla+"nc_num"
	ELSE
		NOTA = IIF(serversql,ttabla+"fact_num",".t.")
	ENDIF

	IF serversql
		tdiv = IIF(tdivide,1,0)
		tca1  = '&ttabla.fact_num, &ttabla_reng.reng_num, &ttabla_reng.reng_num*0  AS com_reng,&ttabla.fec_emis, '
		tca2  = '&ttabla_reng.co_art, art.modelo,art.peso, &ttabla_reng.des_art, &ttabla.comentario, &ttabla.fec_venc, &ttabla.co_cli, '
		tca3  = "&ttabla.co_ven, vendedor.ven_des, &ttabla.&tcampo AS co_tran, &ttabla3.&tcampo2 AS des_tran, &ttabla.contrib, case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla.contrib else clientes.contribu end as contribu, &ttabla.DESCRIP, "
		tca4  = '&ttabla.nombre, &ttabla1.fax, &NOTA AS nc_num, &ttabla.rif as rif_ta, '
		tca5  = '&ttabla.nit as nit_ta, &ttabla1.direc2, &ttabla.forma_pag, &ttabla_reng.uni_venta, &ttabla_reng.co_alma, '
		tca6  = '&ttabla_reng.total_art, &ttabla_reng.tipo_imp, &ttabla_reng.porc_desc,&ttabla_reng.nro_lote, &ttabla.porc_gdesc, &ttabla.porc_reca, '
		tca7  = '&ttabla.dir_ent, &ttabla_reng.comentario AS comentario2, '
		tca8  = '&ttabla_reng.tipo_doc AS rtipo_doc, &ttabla_reng.num_doc AS rnum_doc,condicio.cond_des, '
		tca9  = 'condicio.co_cond, &ttabla1.respons, &ttabla.tot_bruto-&ttabla.glob_desc as sub_des, &ttabla.tasag, &ttabla.tasa, &tvuelto as vuelto, &ttabla.seriales, art.manj_ser, &ttabla.saldo, art.suni_venta, '
		tca10 = '&ttabla_reng.stotal_art, &ttabla1.dir_ent2, &ttabla.campo1, &ttabla.campo2, &ttabla.campo3, &ttabla.campo4, &ttabla.campo5, &ttabla.campo6, &ttabla.campo7, &ttabla.campo8, &ttabla.moneda, &ttabla.tasa, '
		tca10a= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.dir_ent else &ttabla1.direc1 end as direc1, "
		tca10b= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.telefono else &ttabla1.telefonos end as telefonos,"
		tca11 = "case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla_reng.des_art else art.art_des end as art_des, "
		tca12 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nombre else &ttabla1.cli_des end as cli_des, "
		tca13 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.rif else &ttabla1.rif end as rif, "
		tca14 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nit else &ttabla1.nit end as nit, "
		tca15 = "ROUND(&ttabla_reng.prec_vta/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS prec_vta, "
		tca16 = "ROUND(&ttabla_reng.reng_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS reng_neto, "
		tca17 = "ROUND(&ttabla.glob_desc/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS glob_desc, "
		tca18 = "ROUND(&ttabla.tot_reca/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_reca, "
		tca19 = "ROUND(&ttabla.iva/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS iva, "
		tca20 = "ROUND(&ttabla.tot_flete/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_flete, "
		tca21 = "ROUND(&ttabla.tot_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_neto, "
		tca22 = "ROUND(&ttabla.tot_bruto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_bruto, "
		tca23 = "'FACT' AS tipo, space(30) as dir_ent2,space(200) as serial1, &ttabla.co_sucu,&ttabla_reng.mon_ilc,&ttabla_reng.otros, &ttabla.moneda  "

		tfr1  = '&tfrom, &tfrom_reng, art, condicio, vendedor, &tfrom3, &tfrom1'
		twh1  = '&ttabla.fact_num BETWEEN '+ALLTRIM(STR(tdesde[1]))+' AND '+ALLTRIM(STR(thasta[1]))+' AND &ttabla_reng.co_art = art.co_art AND &ttabla.fact_num = &ttabla_reng.fact_num '
		twh2  = 'AND &ttabla.co_cli = &ttabla1.co_cli AND &ttabla.forma_pag = condicio.co_cond AND &ttabla.co_ven = vendedor.co_ven AND &ttabla.&tcampo = &ttabla3.&tcampo AND &ttabla.anulada = 0 '
		tor1  = '&ttabla.fact_num, &ttabla_reng.reng_num '

*!*			IF rtipo = 12
*!*				tca3  = "&ttabla.co_ven, vendedor.ven_des, &ttabla.co_ven as co_tran, vendedor.ven_des as des_tran, &ttabla.contrib, case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla.contrib else clientes.contribu end as contribu, &ttabla.DESCRIP, "
*!*				tfr1  = '&tfrom, &tfrom_reng, art, condicio, vendedor, &tfrom1'
*!*				twh2  = 'AND &ttabla.co_cli = &ttabla1.co_cli AND &ttabla.forma_pag = condicio.co_cond AND &ttabla.co_ven = vendedor.co_ven AND &ttabla.anulada = 0 '
*!*			ENDIF


		tresult=sqlexec(tconnect,'select '+tca1+tca2+tca3+tca4+tca5+tca6+tca7+tca8+tca9+tca10+tca10a+tca10b+tca11+tca12+tca13+tca14+tca15+tca16+tca17+tca18+tca19+tca20+tca21+tca22+tca23+' from '+tfr1+' WHERE '+twh1+twh2+' order by '+tor1,treport)
		IF mensaje_sql(tresult,1,"Error primer sql Ventas") <= 0
			RETURN .F.
		ENDIF

		&& gam 111102 al ejecutar pantalla de formato desde plantilla el cursor anterior, rtipo toma valor 2 por lo q luego
		&& el formato q muestra es el de cotizaciones a cliente			
		rtipo=rtipo_aux
		
		IF vpar_emp.p_imp_kit
			tca1  = '&ttabla.fact_num, &ttabla_reng.reng_num, &ttabla5.reng_num  AS com_reng,&ttabla.fec_emis, '
			tca2  = '&ttabla5.co_art, art.modelo,art.peso, &ttabla_reng.des_art, &ttabla.comentario, &ttabla.fec_venc, &ttabla.co_cli, '
			tca3  = "&ttabla.co_ven, vendedor.ven_des, &ttabla.&tcampo AS co_tran, &ttabla3.&tcampo2 AS des_tran, &ttabla.contrib, case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla.contrib else clientes.contribu end as contribu, &ttabla.DESCRIP, "
			tca4  = '&ttabla.nombre,&ttabla1.fax, &NOTA AS nc_num, &ttabla.rif as rif_ta, '
			tca5  = '&ttabla.nit as nit_ta, &ttabla1.direc2, &ttabla.forma_pag, &ttabla5.uni_venta, &ttabla_reng.co_alma, '
			tca6  = '&ttabla_reng.total_art*&ttabla5.total_art as total_art, &ttabla_reng.tipo_imp, &ttabla_reng.porc_desc, &ttabla_reng.nro_lote, &ttabla.porc_gdesc, &ttabla.porc_reca, '
			tca7  = '&ttabla.dir_ent, &ttabla_reng.comentario AS comentario2, '
			tca8  = '&ttabla_reng.tipo_doc AS rtipo_doc, &ttabla_reng.num_doc AS rnum_doc,condicio.cond_des, '
			tca9  = 'condicio.co_cond, &ttabla1.respons, &ttabla.tot_bruto-&ttabla.glob_desc as sub_des, &ttabla.tasag, &ttabla.tasa, &tvuelto as vuelto, &ttabla.seriales, art.manj_ser, &ttabla.saldo, art.suni_venta, '
			tca10 = '&ttabla_reng.stotal_art, &ttabla1.dir_ent2, &ttabla.campo1, &ttabla.campo2, &ttabla.campo3, &ttabla.campo4, &ttabla.campo5, &ttabla.campo6, &ttabla.campo7, &ttabla.campo8, &ttabla.moneda, &ttabla.tasa, '
			tca10a= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.dir_ent else &ttabla1.direc1 end as direc1, "
		    tca10b= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.telefono else &ttabla1.telefonos end as telefonos,"
			tca11 = "case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla_reng.des_art else art.art_des end as art_des, "
			tca12 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nombre else &ttabla1.cli_des end as cli_des, "
			tca13 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.rif else &ttabla1.rif end as rif, "
			tca14 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nit else &ttabla1.nit end as nit, "
			tca15 = "ROUND(&ttabla_reng.prec_vta/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS prec_vta, "
			tca16 = "ROUND(&ttabla_reng.reng_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS reng_neto, "
			tca17 = "ROUND(&ttabla.glob_desc/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS glob_desc, "
			tca18 = "ROUND(&ttabla.tot_reca/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_reca, "
			tca19 = "ROUND(&ttabla.iva/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS iva, "
			tca20 = "ROUND(&ttabla.tot_flete/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_flete, "
			tca21 = "ROUND(&ttabla.tot_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_neto, "
			tca22 = "ROUND(&ttabla.tot_bruto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_bruto, "
			tca23 = "'COMP' AS tipo,space(30) as dir_ent2, space(200) as serial1, &ttabla.co_sucu,&ttabla_reng.mon_ilc,&ttabla_reng.otros, &ttabla.moneda  "
			tfr1  = '&tfrom, &tfrom_reng, art, condicio, vendedor, &tfrom3, &tfrom1, &tfrom4, &tfrom5'
			twh1  = '&ttabla.fact_num BETWEEN '+ALLTRIM(STR(tdesde[1]))+' AND '+ALLTRIM(STR(thasta[1]))+' AND &ttabla.fact_num= &ttabla_reng.fact_num AND  &ttabla5.co_art   = art.co_art AND  &ttabla4.kit_num  = &ttabla5.kit_num AND  &ttabla_reng.co_art = &ttabla4.co_art '
			twh2  = 'AND &ttabla.co_cli = &ttabla1.co_cli AND &ttabla.forma_pag = condicio.co_cond AND &ttabla.co_ven = vendedor.co_ven AND &ttabla.&tcampo = &ttabla3.&tcampo AND &ttabla.anulada = 0 '
			tor1  = '&ttabla.fact_num, &ttabla_reng.reng_num '

*!*				IF rtipo = 12
*!*					tca3  = "&ttabla.co_ven, vendedor.ven_des, &ttabla.co_ven as co_tran, "+;
*!*					" vendedor.ven_des as des_tran, &ttabla.contrib, "+;
*!*					" case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla.contrib else clientes.contribu end as contribu, &ttabla.DESCRIP, "
*!*					tfr1  = '&tfrom, &tfrom_reng, art, condicio, vendedor, &tfrom1, &tfrom4, &tfrom5'
*!*					twh2  = 'AND &ttabla.co_cli = &ttabla1.co_cli AND &ttabla.forma_pag = condicio.co_cond AND &ttabla.co_ven = vendedor.co_ven AND &ttabla.anulada = 0 '
*!*				ENDIF
			
			tresult=sqlexec(tconnect,'select '+tca1+tca2+tca3+tca4+tca5+tca6+tca7+tca8+tca9+tca10+tca10a+tca10b+tca11+tca12+tca13+tca14+tca15+tca16+tca17+tca18+tca19+tca20+tca21+tca22+tca23+' from '+tfr1+' WHERE '+twh1+twh2+' order by '+tor1,'vrepaux2')
			IF mensaje_sql(tresult,1,"Error EN SEGUNDO SQL Ventas")<=0
				RETURN .F.
			ENDIF

			SELECT vrepaux.*;
				FROM vrepaux;
				INTO CURSOR vreportes;
				UNION ALL SELECT vrepaux2.*;
				FROM vrepaux2;
				ORDER BY 1,2,3

		ENDIF
		
&& cÃƒÂ³digo para buscar los seriales de los renglones q tienen seriales asignados	- GAM 190702	
	IF (rtipo=1 OR rtipo= 3 OR rtipo = 4 OR rtipo = 5)
		ttipo = IIF(rtipo=3,"E","S")
		tbuscark4  = IIF(ttipo="E",ttip_doc,tinicial)
		tbuscark5  = IIF(ttipo="E",ttip_doc,tfinal)
		tbuscark6  = IIF(ttipo="E",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark7  = IIF(ttipo="E",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))
		tbuscark8  = IIF(ttipo="S",ttip_doc,tinicial)
		tbuscark9  = IIF(ttipo="S",ttip_doc,tfinal)
		tbuscark10 = IIF(ttipo="S",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark11 = IIF(ttipo="S",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))

		&& c buscan los seriales asignados al doc
		lca1 = "Seriales.doc_num_s, Seriales.co_art, Seriales.serial "
		lfr1 = "seriales"
		lwh1 = " Seriales.doc_tip_e BETWEEN '"+tbuscark4+"' and '"+tbuscark5+"'"
		lwh2 = " AND Seriales.doc_num_e BETWEEN "+tbuscark6+" AND "+tbuscark7
		lwh3 = " AND Seriales.doc_tip_s BETWEEN '"+tbuscark8+"' and '"+tbuscark9+"'"
		lwh4 = " AND Seriales.doc_num_s BETWEEN "+tbuscark10+" and "+tbuscark11 
		lor1 = " Seriales.doc_num_s, Seriales.co_art, Seriales.contador"
		
		tresult=sqlexec(tconnect,'select '+lca1+' from '+lfr1+' WHERE '+lwh1+lwh2+lwh3+lwh4+' order by '+lor1,'vrepauxp')
			IF mensaje_sql(tresult,1,"Error consultando la tabla de seriales")<=0
				RETURN .F.
			ENDIF
		lexiste_ser = !eof('vrepauxp')
		if lexiste_ser   && si existen seriales. c busca mediante bus_serial cuales renglones tienen seriales asignados
		 SELECT vreportes.*,; 
			LEFT(bus_serial(vreportes.co_art,IIF(rtipo=5,vreportes.rnum_doc,vreportes.fact_num),IIF(rtipo=3,"E","S"),ttip_doc,0),250) AS serial2;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes
						&& c crea el cursor temporal donde c almacenaran los seriales de c/u renglon		
		 **CREATE CURSOR TEMP_SERIAL (fact_num N(4), reng_num N(4), SERIAL_TEMP M)
		 ** && AD: 09-08-2002  No se porque se creo el fact_num como numerico y no entero preguntar a samper
		 CREATE CURSOR TEMP_SERIAL (fact_num I, reng_num I, SERIAL_TEMP M)
		 ***************************************************************************
		 SELECT VREPAUXP
		 GO TOP
		 SELECT VREPORTES.* FROM VREPORTES ORDER BY FACT_NUM, CO_ART INTO CURSOR VREPORTES1  && c ordena vreportes por co_art
		 GO TOP
						&& c recorren los renglones en vreportes1 y c le asigna a c/u su correspondiente serial del  cursor vrepuxup		
						
		 DO WHILE !EOF('VREPORTES1')
		  LSERIAL_ART = ''	
		  IF !EMPTY(alltrim(VREPORTES1.SERIAL2))  AND (VREPORTES1.FACT_NUM=VREPAUXP.DOC_NUM_S) && posee serial asignado
		    LCANT_SACT = VREPORTES1.TOTAL_ART  && c agregan los seriales segun total_art
		    FOR I=1 TO LCANT_SACT
		      LSERIAL_ART = LSERIAL_ART+ALLTRIM(vrepauxp.SERIAL)+' '
		      IF !EOF('VREPAUXP')
		       SKIP 1 IN vrepauxp
		      ENDIF
		      IF !(VREPAUXP.CO_ART==VREPORTES1.CO_ART)
		       EXIT
		      ENDIF 
		    NEXT
		    SELECT TEMP_SERIAL
		    APPEND BLAN  && c agrega el nro del renglon y el serial
		    REPLACE SERIAL_TEMP WITH alltrim(LSERIAL_ART),;
		    	  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		  ELSE && no posee serial asignado
		    SELECT TEMP_SERIAL
		    APPEND BLAN
		    REPLACE SERIAL_TEMP WITH '',;
		    	  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		  	IF VREPORTES1.CO_ART == VREPAUXP.CO_ART
		  	 SKIP VREPORTES1.TOTAL_ART IN VREPAUXP	  		  
		  	ENDIF 
		  ENDIF
		  SKIP IN VREPORTES1
		 ENDDO
		 && c efectua el enlace entre vreportes y el cursor temporal de seriales
		 SELECT vreportes.*,TEMP_SERIAL.SERIAL_TEMP AS SERIAL FROM VREPORTES,TEMP_SERIAL ;
		 	WHERE VREPORTES.fact_num=TEMP_SERIAL.fact_num AND VREPORTES.reng_num=TEMP_SERIAL.reng_num ORDER BY 1,2,3 INTO CURSO VREPORTES
		else  && no existen serailes asignados
		 SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes
		endif 
   	ELSE
   	 SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes
   	ENDIF		
	&& fin cÃƒÂ³digo de manejo seriales
	
	ELSE

		lturno=IIF(RTIPO=0 OR RTIPO=1,ttabla+"NUM_TURNO","0")
		SELECT &ttabla.fact_num,;
			&ttabla_reng.reng_num,;
			tcero AS com_reng,;
			&ttabla.fec_emis,;
			&ttabla_reng.co_art,;
			art.modelo,art.peso,;
			IIF(es_art_gene(&ttabla_reng.co_art),ALLTRIM(SUBSTR(&ttabla_reng.des_art,1,60)),art.art_des) AS art_des,;
			&ttabla.comentario,;
			&ttabla.fec_venc,;
			&ttabla.co_cli,;
			&ttabla.co_ven,;
			vendedor.ven_des,;
			&ttabla.&tcampo AS co_tran,;
			&ttabla3.&tcampo2 AS des_tran,;
			&ttabla.contrib,;
			IIF(es_cli_gene(&ttabla.co_cli), &ttabla.contrib, &ttabla1.contribu) AS contribu,;
			&ttabla.DESCRIP,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.nombre+space(100-len(alltrim(&ttabla.nombre))),&ttabla1.cli_des) AS cli_des,;
			&ttabla1.fax,;
			&NOTA AS nc_num,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.rif,&ttabla1.rif) AS rif,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.nit,&ttabla1.nit) AS nit,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.dir_ent,&ttabla1.direc1) AS direc1,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.telefono,&ttabla1.telefonos) AS telefonos,;
			&ttabla1.direc2,;
			&ttabla.forma_pag,;
			&ttabla_reng.uni_venta,;
			&ttabla_reng.co_alma,;
			&ttabla_reng.total_art,;
			&ttabla_reng.tipo_imp,;
			&ttabla_reng.porc_desc,;
			&ttabla_reng.nro_lote,;
			ROUND(&ttabla_reng.prec_vta/IIF(tdivide,1,&ttabla.tasa), 2) AS prec_vta,;
			ROUND(&ttabla_reng.reng_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS reng_neto,;
			&ttabla.porc_gdesc,;
			&ttabla.porc_reca,;
			ROUND(&ttabla.glob_desc/IIF(tdivide,1,&ttabla.tasa), 2) AS glob_desc,;
			ROUND(&ttabla.tot_reca/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_reca,;
			ROUND(&ttabla.iva/IIF(tdivide,1,&ttabla.tasa), 2) AS iva,;
			ROUND(&ttabla.tot_flete/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_flete,;
			&ttabla.dir_ent,;
			&ttabla_reng.comentario AS comentario2,;
			&ttabla_reng.tipo_doc AS rtipo_doc,;
			&ttabla_reng.num_doc AS rnum_doc,;
			ROUND(&ttabla.tot_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_neto,;
			ROUND(&ttabla.tot_bruto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_bruto,;
			condicio.cond_des,;
			condicio.co_cond,;
			"FACT" AS tipo,;
			&ttabla1.respons,;
			&ttabla.tot_bruto-&ttabla.glob_desc AS sub_des,;
			&ttabla.tasag,;
			&ttabla.tasa,;
			IIF((rtipo=1 OR rtipo= 3 OR rtipo = 4 OR rtipo = 5),LEFT(bus_serial(&ttabla_reng.co_art,IIF(rtipo=5,&ttabla_reng.num_doc,&ttabla.fact_num),IIF(rtipo=3,"E","S"),ttip_doc,0),250), SPACE(1)) AS serial2,;
			&tvuelto AS vuelto,;
			&ttabla.seriales,;
			art.manj_ser,;
			&ttabla.saldo,;
			art.suni_venta,;
			&ttabla_reng.stotal_art,;
			&ttabla1.dir_ent2,;
			&ttabla.campo1,;
			&ttabla.campo2,;
			&ttabla.campo3,;
			&ttabla.campo4,;
			&ttabla.campo5,;
			&ttabla.campo6,;
			&ttabla.campo7,;
			&ttabla.campo8,;
			&ttabla.co_sucu,;
			&ttabla_reng.mon_ilc,;
			&ttabla_reng.otros, &ttabla.moneda,&lturno AS TURNO;		
			FROM bldatos!&tfrom, bldatos!&tfrom_reng,art,bldatos!condicio,;
			bldatos!vendedor,bldatos!&tfrom3, bldatos!&tfrom1;
			WHERE &ttabla.fact_num BETWEEN ?tdesde[1] AND ?thasta[1];
			AND &ttabla_reng.co_art = art.co_art;
			AND &ttabla.fact_num    = &ttabla_reng.fact_num;
			AND &ttabla.co_cli      = &ttabla1.co_cli;
			AND &ttabla.forma_pag = condicio.co_cond;
			AND &ttabla.co_ven    = vendedor.co_ven;
			AND &ttabla.&tcampo   = &ttabla3.&tcampo;
			AND &ttabla.anulada = .F.;
			GROUP BY 1,2 ORDER BY 1,2 INTO CURSO &treport

&& gam 111102 al ejecutar pantalla de formato desde plantilla el cursor anterior, rtipo toma valor 2 por lo q luego
&& el formato q muestra es el de cotizaciones a cliente			
rtipo=rtipo_aux

		IF vpar_emp.p_imp_kit
			SELECT *;
				FROM vrepaux;
				INTO CURSO vreportes;
				UNION ALL SELECT &ttabla.fact_num,;
				&ttabla_reng.reng_num,;
				&ttabla5.reng_num AS com_reng,;
				&ttabla.fec_emis,;
				&ttabla5.co_art,;
				art.modelo,art.peso,;
				IIF(es_art_gene(&ttabla_reng.co_art),ALLTRIM(SUBSTR(&ttabla_reng.des_art,1,60)),art.art_des) AS art_des,;
				&ttabla.comentario,;
				&ttabla.fec_venc,;
				&ttabla.co_cli,;
				&ttabla.co_ven,;
				vendedor.ven_des,;
				&ttabla.&tcampo AS co_tran,;
				&ttabla3.&tcampo2 AS des_tran,;
				&ttabla.contrib,;
				IIF(es_cli_gene(&ttabla.co_cli), &ttabla.contrib, &ttabla1.contribu) AS contribu,;
				&ttabla.DESCRIP,;
				&ttabla1.cli_des,;
				&ttabla1.fax,;
				&ttabla1.telefonos,;
				&NOTA AS nc_num,;
				&ttabla1.rif,;
				&ttabla1.nit,;
				&ttabla1.direc1,;
				&ttabla1.direc2,;
				&ttabla.forma_pag,;
				&ttabla5.uni_venta,;
				&ttabla_reng.co_alma,;
				(&ttabla_reng.total_art*&ttabla5.total_art) AS total_art,;
				&ttabla_reng.tipo_imp,;
				&ttabla_reng.porc_desc,;
				&ttabla_reng.nro_lote,;
				ROUND(&ttabla_reng.prec_vta/IIF(tdivide,1,&ttabla.tasa), 2) AS prec_vta,;
				ROUND(&ttabla_reng.reng_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS reng_neto,;
				&ttabla.porc_gdesc,;
				&ttabla.porc_reca,;
				ROUND(&ttabla.glob_desc/IIF(tdivide,1,&ttabla.tasa), 2) AS glob_desc,;
				ROUND(&ttabla.tot_reca/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_reca,;
				ROUND(&ttabla.iva/IIF(tdivide,1,&ttabla.tasa), 2) AS iva,;
				ROUND(&ttabla.tot_flete/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_flete,;
				&ttabla.dir_ent,;
				&ttabla_reng.comentario AS comentario2,;
				&ttabla_reng.tipo_doc AS rtipo_doc,;
				&ttabla_reng.num_doc AS rnum_doc,;
				ROUND(&ttabla.tot_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_neto,;
				ROUND(&ttabla.tot_bruto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_bruto,;
				condicio.cond_des,;
				condicio.co_cond,;
				"COMP" AS tipo,;
				&ttabla1.respons,;
				&ttabla.tot_bruto-&ttabla.glob_desc AS sub_des,;
				&ttabla.tasag,;
				&ttabla.tasa,;
				IIF((rtipo=1 OR rtipo= 3 OR rtipo = 4 OR rtipo = 5),LEFT(bus_serial(&ttabla5.co_art,IIF(rtipo=5,&ttabla_reng.num_doc,&ttabla.fact_num),IIF(rtipo=3,"E","S"),ttip_doc,0),250), SPACE(1)) AS serial2,;
				&tvuelto AS vuelto,;
				&ttabla.seriales,;
				art.manj_ser,;
				&ttabla.saldo,;
				art.suni_venta,;
				&ttabla_reng.stotal_art,;
				&ttabla1.dir_ent2,;
				&ttabla.campo1,;
				&ttabla.campo2,;
				&ttabla.campo3,;
				&ttabla.campo4,;
				&ttabla.campo5,;
				&ttabla.campo6,;
				&ttabla.campo7,;
				&ttabla.campo8,;
				&ttabla.co_sucu,;
			&ttabla_reng.mon_ilc,;
			&ttabla_reng.otros, &ttabla.moneda,&lturno AS TURNO;		
				FROM bldatos!&tfrom_reng,bldatos!&tfrom4,bldatos!&tfrom5,art,;
				bldatos!&tfrom1, bldatos!vendedor, bldatos!condicio,;
				bldatos!&tfrom,bldatos!&tfrom3;
				WHERE &ttabla.fact_num BETWEEN ?tdesde[1] AND ?thasta[1];
				AND  &ttabla5.co_art   = art.co_art;
				AND  &ttabla4.kit_num  = &ttabla5.kit_num;
				AND  &ttabla_reng.co_art = &ttabla4.co_art;
				AND  &ttabla.fact_num  = &ttabla_reng.fact_num;
				AND  &ttabla.co_cli    = &ttabla1.co_cli;
				AND  &ttabla.forma_pag = condicio.co_cond;
				AND  &ttabla.co_ven    = vendedor.co_ven;
				AND  &ttabla.anulada   = .F.;
				AND &ttabla.&tcampo = &ttabla3.&tcampo;
				GROUP BY 1,2,3 ORDER BY 1,2,3
		ENDIF
			 && cÃƒÂ³digo para buscar los seriales de los renglones q tienen seriales asignados	- GAM 190702
	
	 IF (rtipo=1 OR rtipo= 3 OR rtipo = 4 OR rtipo = 5)
		ttipo = IIF(rtipo=3,"E","S")
		tbuscark4  = IIF(ttipo="E",ttip_doc,tinicial)
		tbuscark5  = IIF(ttipo="E",ttip_doc,tfinal)
		tbuscark6  = IIF(ttipo="E",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark7  = IIF(ttipo="E",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))
		tbuscark8  = IIF(ttipo="S",ttip_doc,tinicial)
		tbuscark9  = IIF(ttipo="S",ttip_doc,tfinal)
		tbuscark10 = IIF(ttipo="S",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark11 = IIF(ttipo="S",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))

		&& c buscan los seriales asignados al doc
		lca1 = "Seriales.co_art, Seriales.serial, Seriales.doc_num_s "
		lfr1 = "seriales"
		lwh1 = " Seriales.doc_tip_e BETWEEN '"+tbuscark4+"' and '"+tbuscark5+"'"
		lwh2 = " AND Seriales.doc_num_e BETWEEN "+tbuscark6+" AND "+tbuscark7
		lwh3 = " AND Seriales.doc_tip_s BETWEEN '"+tbuscark8+"' and '"+tbuscark9+"'"
		lwh4 = " AND Seriales.doc_num_s BETWEEN "+tbuscark10+" and "+tbuscark11 
		lor1 = " Seriales.doc_num_s, Seriales.co_art"
		
		lcadena='select '+lca1+' from '+lfr1+' WHERE '+lwh1+lwh2+lwh3+lwh4+' order by '+lor1+' into cursor vrepauxp'
		&lcadena

	 	lexiste_ser = !(_TALLY=0)
	    && si existen seriales. c busca mediante bus_serial cuales renglones tienen seriales asignados	
	  IF lexiste_ser	
	    && c crea el cursor temporal donde c almacenaran los seriales de c/u renglon		
	    **CREATE CURSOR TEMP_SERIAL (fact_num N(4), reng_num N(4), SERIAL_TEMP M)
		 ** && AD: 09-08-2002  No se porque se creo el fact_num como numerico y no entero preguntar a samper
		CREATE CURSOR TEMP_SERIAL (fact_num I, reng_num I, SERIAL_TEMP M)
		********************************************
		SELECT VREPAUXP
		GO TOP
		SELECT VREPORTES.* FROM VREPORTES ORDER BY FACT_NUM, CO_ART INTO CURSOR VREPORTES1
		GO TOP

		&& c recorren los renglones en vreportes1 y c le asigna a c/u su correspondiente serial del  cursor vrepuxup		
		DO WHILE !EOF('VREPORTES1')
		 LSERIAL_ART = ''	
		 IF !EMPTY(ALLTRIM(VREPORTES1.SERIAL2)) AND (VREPORTES1.FACT_NUM=VREPAUXP.DOC_NUM_S) && posee serial
		  LCANT_SACT = VREPORTES1.TOTAL_ART  && c agregan los seriales segun total_art
		  FOR I=1 TO LCANT_SACT
		   LSERIAL_ART = LSERIAL_ART+ALLTRIM(vrepauxp.SERIAL)+' '
		   IF !EOF('VREPAUXP')
		    SKIP 1 IN vrepauxp
		   ENDIF
		   IF !(VREPAUXP.CO_ART==VREPORTES1.CO_ART)
		      EXIT
	       ENDIF  
		  NEXT
		  SELECT TEMP_SERIAL
		  APPEND BLAN       && c agrega el nro del renglon y el serial
		  REPLACE SERIAL_TEMP WITH alltrim(LSERIAL_ART),;
		  		  fact_num WITH VREPORTES1.FACT_NUM,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		 ELSE  && no posee serial
		  SELECT TEMP_SERIAL
		  APPEND BLAN
		  REPLACE SERIAL_TEMP WITH '',;
		  		  fact_num WITH VREPORTES1.FACT_NUM,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		  IF VREPORTES1.CO_ART == VREPAUXP.CO_ART
		   SKIP VREPORTES1.TOTAL_ART IN VREPAUXP		  		  
		  ENDIF 
		 ENDIF
		 SKIP IN VREPORTES1
		ENDDO
		&& c efectua el enlace entre vreportes y el cursor temporal de seriales
		SELECT vreportes.*,TEMP_SERIAL.SERIAL_TEMP AS SERIAL FROM VREPORTES,TEMP_SERIAL ;
		 WHERE VREPORTES.fact_num=TEMP_SERIAL.fact_num AND VREPORTES.reng_num=TEMP_SERIAL.reng_num ORDER BY 1,2,3 INTO CURSO VREPORTES
	  ELSE  && no existe seriales
	    SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	  ENDIF	 
	 	 
	 ELSE
	  SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	 ENDIF	
	&& fin dÃƒÂ³digo de manejo seriales	
	ENDIF
ELSE
	IF rtipo = 9
		NOTA = ttabla+"nc_num"
	ELSE
		IF serversql
			NOTA = ttabla+"fact_num"
		ELSE
			NOTA = ".t."
		ENDIF
	ENDIF
	IF rtipo = 7 OR rtipo = 10
		fac = ttabla+"factura"
	ELSE
		IF serversql
			fac = ttabla+"fact_num"
		ELSE
			fac = ".t."
		ENDIF
	ENDIF
	IF serversql
		tdiv = IIF(tdivide,1,0)
		tca1  = '&ttabla.fact_num, &ttabla_reng.reng_num, &ttabla_reng.reng_num*0  AS com_reng,&ttabla.fec_emis, '
		tca2  = '&ttabla_reng.co_art, art.modelo,art.peso, &ttabla_reng.des_art, &ttabla.comentario, &ttabla.fec_venc, &ttabla.co_cli, '
		tca3  = '&ttabla.contrib,&ttabla.DESCRIP, &ttabla.nombre, &ttabla1.fax, &NOTA AS nc_num, &ttabla.rif as rif_ta, '
		tca4  = '&ttabla.nit as nit_ta, &ttabla1.direc2, &ttabla.forma_pag, &ttabla_reng.uni_venta, &ttabla_reng.co_alma, '
		tca5  = '&ttabla_reng.total_art, &ttabla_reng.tipo_imp, &ttabla_reng.porc_desc, &ttabla_reng.nro_lote, &ttabla.porc_gdesc, &ttabla.porc_reca, '
		tca6  = '&ttabla.dir_ent, &ttabla_reng.comentario AS comentario2, '
		tca7  = '&ttabla_reng.tipo_doc AS rtipo_doc, &ttabla_reng.num_doc AS rnum_doc, condicio.cond_des, '
		tca8  = 'condicio.co_cond, &fac as factura, &ttabla1.respons, &ttabla.tot_bruto-&ttabla.glob_desc as sub_des, &ttabla.tasag, &ttabla.tasa, &tvuelto as vuelto, &ttabla.seriales, art.manj_ser, &ttabla.saldo, art.suni_venta, '
		tca9  = '&ttabla_reng.stotal_art, &ttabla.campo1, &ttabla.campo2, &ttabla.campo3, &ttabla.campo4, &ttabla.campo5, &ttabla.campo6, &ttabla.campo7, &ttabla.campo8, &ttabla.moneda, &ttabla.tasa, '
		tca10 = "case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla_reng.des_art else art.art_des end as art_des, "
		tca10a= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.dir_ent else &ttabla1.direc1 end as direc1, "
		tca10b= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.telefono else &ttabla1.telefonos end as telefonos,"
		tca11 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nombre else &ttabla1.prov_des end as cli_des, "
		tca12 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.co_cli else &ttabla1.rif end as rif, "
		tca13 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.co_cli else &ttabla1.nit end as nit, "
		tca14 = "ROUND(&ttabla_reng.prec_vta/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS prec_vta, "
		tca15 = "ROUND(&ttabla_reng.reng_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS reng_neto, "
		tca16 = "ROUND(&ttabla.glob_desc/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS glob_desc, "
		tca17 = "ROUND(&ttabla.tot_reca/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_reca, "
		tca18 = "ROUND(&ttabla.iva/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS iva, "
		tca19 = "ROUND(&ttabla.tot_flete/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_flete, "
		tca20 = "ROUND(&ttabla.tot_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_neto, "
		tca21 = "ROUND(&ttabla.tot_bruto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_bruto, "
		tca22 = "'FACT' AS tipo, space(30) as dir_ent2,space(200) as serial1, &ttabla.co_sucu,&ttabla.moneda "

		tfr1  = 'condicio, &tfrom1 right outer join &tfrom left outer join &tfrom_reng left outer join art '
		tfr2  = 'on &ttabla_reng.co_art = art.co_art on &ttabla.fact_num = &ttabla_reng.fact_num on &ttabla.co_cli = &ttabla1.co_prov '		
		twh1  = '&ttabla.anulada = 0 AND &ttabla.forma_pag = condicio.co_cond '  &&& AD : se quito esto AND &ttabla.co_tran = &ttabla3.co_tran '
		
		twh2  = 'AND &ttabla.fact_num BETWEEN '+ALLTRIM(STR(tdesde[1]))+' AND '+ALLTRIM(STR(thasta[1]))+' '
		tor1  = '&ttabla.fact_num, &ttabla_reng.reng_num '

		tresult=sqlexec(tconnect,'select '+tca1+tca2+tca3+tca4+tca5+tca6+tca7+tca8+tca9+tca10+tca10a+tca10b+tca11+tca12+tca13+tca14+tca15+tca16+tca17+tca18+tca19+tca20+tca21+tca22+' from '+tfr1+tfr2+' WHERE '+twh1+twh2+' order by '+tor1,treport)
		IF mensaje_sql(tresult,1,"Error primer sql Compras") <= 0
			RETURN .F.
		ENDIF


		IF vpar_emp.p_imp_kit
			tca1  = '&ttabla.fact_num, &ttabla_reng.reng_num, &ttabla5.reng_num  AS com_reng,&ttabla.fec_emis, '
			tca2  = '&ttabla5.co_art, art.modelo,art.peso, &ttabla_reng.des_art, &ttabla.comentario, &ttabla.fec_venc, &ttabla.co_cli, '
			tca3  = '&ttabla.contrib,&ttabla.DESCRIP, &ttabla.nombre, &ttabla1.fax, &NOTA AS nc_num, &ttabla.rif as rif_ta, '
			tca4  = '&ttabla.nit as nit_ta, &ttabla1.direc2, &ttabla.forma_pag, &ttabla5.uni_venta, &ttabla_reng.co_alma, '
			tca5  = '&ttabla_reng.total_art*&ttabla5.total_art as total_art, &ttabla_reng.tipo_imp, &ttabla_reng.porc_desc, &ttabla_reng.nro_lote, &ttabla.porc_gdesc, &ttabla.porc_reca, '
			tca6  = '&ttabla.dir_ent, &ttabla_reng.comentario AS comentario2, '
			tca7  = '&ttabla_reng.tipo_doc AS rtipo_doc, &ttabla_reng.num_doc AS rnum_doc,condicio.cond_des, '
			tca8  = 'condicio.co_cond, &fac as factura, &ttabla1.respons, &ttabla.tot_bruto-&ttabla.glob_desc as sub_des, &ttabla.tasag, &ttabla.tasa, &tvuelto as vuelto, &ttabla.seriales, art.manj_ser, &ttabla.saldo, art.suni_venta, '
			tca9  = '&ttabla_reng.stotal_art, &ttabla.campo1, &ttabla.campo2, &ttabla.campo3, &ttabla.campo4, &ttabla.campo5, &ttabla.campo6, &ttabla.campo7, &ttabla.campo8, &ttabla.moneda, &ttabla.tasa, '
			tca10 = "case when substring(&ttabla_reng.co_art,1,3) = 'GEN' then &ttabla_reng.des_art else art.art_des end as art_des, "
			tca10a= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.dir_ent else &ttabla1.direc1 end as direc1, "
     		tca10b= "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.telefono else &ttabla1.telefonos end as telefonos,"
			tca11 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.nombre else &ttabla1.prov_des end as cli_des, "
			tca12 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.co_cli else &ttabla1.rif end as rif, "
			tca13 = "case when substring(&ttabla.co_cli,1,3) = 'GEN' then &ttabla.co_cli else &ttabla1.nit end as nit, "
			tca14 = "ROUND(&ttabla_reng.prec_vta/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS prec_vta, "
			tca15 = "ROUND(&ttabla_reng.reng_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS reng_neto, "
			tca16 = "ROUND(&ttabla.glob_desc/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS glob_desc, "
			tca17 = "ROUND(&ttabla.tot_reca/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_reca, "
			tca18 = "ROUND(&ttabla.iva/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS iva, "
			tca19 = "ROUND(&ttabla.tot_flete/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_flete, "
			tca20 = "ROUND(&ttabla.tot_neto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_neto, "
			tca21 = "ROUND(&ttabla.tot_bruto/case when "+STR(tdiv)+"=1 then 1 else &TTABLA.TASA END, 2) AS tot_bruto, "
			tca22 = "'COMP' AS tipo,space(30) as dir_ent2,space(200) as serial1, &ttabla.co_sucu,&ttabla.moneda "

			tfr1  = '&tfrom1,condicio,&tfrom inner join &tfrom_reng inner join &tfrom4 inner join &tfrom5 inner join art '
			tfr2  = 'on &ttabla5.co_art = art.co_art on &ttabla4.kit_num = &ttabla5.kit_num on &ttabla_reng.co_art = &ttabla4.co_art on &ttabla.fact_num = &ttabla_reng.fact_num '
			twh1  = '&ttabla.anulada = 0 AND &ttabla.co_cli = &ttabla1.co_prov AND &ttabla.forma_pag = condicio.co_cond AND &ttabla.fact_num BETWEEN '+ALLTRIM(STR(tdesde[1]))+' AND '+ALLTRIM(STR(thasta[1]))+' '
			tor1  = '&ttabla.fact_num, &ttabla_reng.reng_num '
			
&&AND &ttabla.co_tran = &ttabla3.co_tran

			tresult=sqlexec(tconnect,'select '+tca1+tca2+tca3+tca4+tca5+tca6+tca7+tca8+tca9+tca10+tca10a+tca10b+tca11+tca12+tca13+tca14+tca15+tca16+tca17+tca18+tca19+tca20+tca21+tca22+' from '+tfr1+tfr2+' WHERE '+twh1+' order by '+tor1,'vrepaux2')
			IF mensaje_sql(tresult,1,"Error EN SEGUNDO SQL Compras")<=0
				RETURN .F.
			ENDIF

			SELECT vrepaux.*;
				FROM vrepaux;
				INTO CURSOR vreportes;
				UNION ALL SELECT vrepaux2.*;
				FROM vrepaux2;
				ORDER BY 1,2,3
		ENDIF
		
&& cÃƒÂ³digo para buscar los seriales de los renglones q tienen seriales asignados	- GAM 190702
	 IF (rtipo = 7 OR rtipo = 9 OR rtipo = 10)
		ttipo = IIF(rtipo=9,"S","E")
		tbuscark4  = IIF(ttipo="E",ttip_doc,tinicial)
		tbuscark5  = IIF(ttipo="E",ttip_doc,tfinal)
		tbuscark6  = IIF(ttipo="E",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark7  = IIF(ttipo="E",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))
		tbuscark8  = IIF(ttipo="S",ttip_doc,tinicial)
		tbuscark9  = IIF(ttipo="S",ttip_doc,tfinal)
		tbuscark10 = IIF(ttipo="S",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark11 = IIF(ttipo="S",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))

		&& c buscan los seriales asignados al doc
		lca1 = "Seriales.doc_num_e, Seriales.co_art, Seriales.serial "
		lfr1 = "seriales"
		lwh1 = " Seriales.doc_tip_e BETWEEN '"+tbuscark4+"' and '"+tbuscark5+"'"
		lwh2 = " AND Seriales.doc_num_e BETWEEN "+tbuscark6+" AND "+tbuscark7
		lwh3 = " AND Seriales.doc_tip_s BETWEEN '"+tbuscark8+"' and '"+tbuscark9+"'"
		lwh4 = " AND Seriales.doc_num_s BETWEEN "+tbuscark10+" and "+tbuscark11 
		lor1 = " Seriales.doc_num_e, Seriales.co_art, Seriales.contador"
		
		tresult=sqlexec(tconnect,'select '+lca1+' from '+lfr1+' WHERE '+lwh1+lwh2+lwh3+lwh4+' order by '+lor1,'vrepauxp')
			IF mensaje_sql(tresult,1,"Error consultando la tabla de seriales")<=0
				RETURN .F.
			ENDIF
	 	lexiste_ser = !eof('vrepauxp')
	    && si existen seriales. c busca mediante bus_serial cuales renglones tienen seriales asignados	
	  IF lexiste_ser	
		SELECT vreportes.*,;
			LEFT(bus_serial(vreportes.co_art,vreportes.fact_num,IIF(rtipo=9,"S","E"),ttip_doc,0),250) AS serial2;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes
	    && c crea el cursor temporal donde c almacenaran los seriales de c/u renglon		
	    **CREATE CURSOR TEMP_SERIAL (fact_num N(4), reng_num N(4), SERIAL_TEMP M)
		 ** && AD: 09-08-2002  No se porque se creo el fact_num como numerico y no entero preguntar a samper
		CREATE CURSOR TEMP_SERIAL (fact_num I, reng_num I, SERIAL_TEMP M)
		**************************************************************************
		SELECT VREPAUXP
		GO TOP
		SELECT VREPORTES.* FROM VREPORTES ORDER BY FACT_NUM, CO_ART INTO CURSOR VREPORTES1
		GO TOP
		&& c recorren los renglones en vreportes1 y c le asigna a c/u su correspondiente serial del  cursor vrepuxup		
	
		DO WHILE !EOF('VREPORTES1')
		 LSERIAL_ART = ''	
		 IF !EMPTY(ALLTRIM(VREPORTES1.SERIAL2)) AND (VREPORTES1.FACT_NUM=VREPAUXP.DOC_NUM_E) && posee serial
		  LCANT_SACT = VREPORTES1.TOTAL_ART  && c agregan los seriales segun total_art
		  FOR I=1 TO LCANT_SACT
		   LSERIAL_ART = LSERIAL_ART+ALLTRIM(vrepauxp.SERIAL)+' '
		   IF !EOF('VREPAUXP')
		    SKIP 1 IN vrepauxp
		   ENDIF 
		  NEXT
		  SELECT TEMP_SERIAL
		  APPEND BLAN       && c agrega el nro del renglon y el serial
		  REPLACE SERIAL_TEMP WITH alltrim(LSERIAL_ART),;
		  		  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		 ELSE  && no posee serial
		  SELECT TEMP_SERIAL
		  APPEND BLAN
		  REPLACE SERIAL_TEMP WITH '',;
	  		   	  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		  IF VREPORTES1.CO_ART == VREPAUXP.CO_ART
		   SKIP VREPORTES1.TOTAL_ART IN VREPAUXP		  		  
		  ENDIF 
		 ENDIF
		 SKIP IN VREPORTES1
		ENDDO
		&& c efectua el enlace entre vreportes y el cursor temporal de seriales
		SELECT vreportes.*,TEMP_SERIAL.SERIAL_TEMP AS SERIAL FROM VREPORTES,TEMP_SERIAL ;
		 WHERE VREPORTES.fact_num=TEMP_SERIAL.fact_num AND VREPORTES.reng_num=TEMP_SERIAL.reng_num ORDER BY 1,2,3 INTO CURSO VREPORTES
	  ELSE  && no existe seriales
	    SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	  ENDIF	 
	 	 
	 ELSE
	  SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	 ENDIF	
	&& fin dÃƒÂ³digo de manejo seriales	
	ELSE
		
		SELECT &ttabla.fact_num,;
			&ttabla_reng.reng_num,;
			tcero AS com_reng,;
			&ttabla.fec_emis,;
			&ttabla_reng.co_art,;
			art.modelo,art.peso,;
			IIF(es_art_gene(&ttabla_reng.co_art),ALLTRIM(SUBSTR(&ttabla_reng.des_art,1,60)),art.art_des) AS art_des,;
			&ttabla.comentario,;
			&ttabla.fec_venc,;
			&ttabla.co_cli,;
			&ttabla.contrib,;
			&ttabla.DESCRIP,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.nombre+space(100-len(alltrim(&ttabla.nombre))),&ttabla1.prov_des) AS cli_des,;
			&ttabla1.fax,;
			&NOTA AS nc_num,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.rif,&ttabla1.rif) AS rif,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.nit,&ttabla1.nit) AS nit,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.dir_ent,&ttabla1.direc1) AS direc1,;
			IIF(es_cli_gene(&ttabla.co_cli),&ttabla.telefono,&ttabla1.telefonos) AS telefonos,;
			&ttabla1.direc2,;
			&ttabla.forma_pag,;
			&ttabla_reng.uni_venta,;
			&ttabla_reng.co_alma,;
			&ttabla_reng.total_art,;
			&ttabla_reng.tipo_imp,;
			&ttabla_reng.porc_desc,;
			&ttabla_reng.nro_lote,;
			ROUND(&ttabla_reng.prec_vta/IIF(tdivide,1,&ttabla.tasa), 2) AS prec_vta,;
			ROUND(&ttabla_reng.reng_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS reng_neto,;
			&ttabla.porc_gdesc,;
			&ttabla.porc_reca,;
			ROUND(&ttabla.glob_desc/IIF(tdivide,1,&ttabla.tasa), 2) AS glob_desc,;
			ROUND(&ttabla.tot_reca/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_reca,;
			ROUND(&ttabla.iva/IIF(tdivide,1,&ttabla.tasa), 2) AS iva,;
			ROUND(&ttabla.tot_flete/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_flete,;
			&ttabla.dir_ent,;
			&ttabla_reng.comentario AS comentario2,;
			&ttabla_reng.tipo_doc AS rtipo_doc,;
			&ttabla_reng.num_doc AS rnum_doc,;
			ROUND(&ttabla.tot_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_neto,;
			ROUND(&ttabla.tot_bruto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_bruto,;
			condicio.cond_des,;
			condicio.co_cond,;
			&fac AS factura,;
			"FACT" AS tipo,;
			&ttabla1.respons,;
			&ttabla.tot_bruto-&ttabla.glob_desc AS sub_des,;
			&ttabla.tasag,;
			&ttabla.tasa,;
			IIF((rtipo = 7 OR rtipo = 9 OR rtipo = 10), LEFT(bus_serial(&ttabla_reng.co_art,&ttabla.fact_num,IIF(rtipo=9,"S","E"),ttip_doc,0),250), SPACE(1)) AS serial2,;
			&tvuelto AS vuelto,;
			&ttabla.seriales,;
			art.manj_ser,;
			&ttabla.saldo,;
			art.suni_venta,;
			&ttabla_reng.stotal_art,;
			SPACE(30) AS dir_ent2,;
			&ttabla.campo1,;
			&ttabla.campo2,;
			&ttabla.campo3,;
			&ttabla.campo4,;
			&ttabla.campo5,;
			&ttabla.campo6,;
			&ttabla.campo7,;
			&ttabla.campo8,;
			&ttabla.co_sucu, &ttabla.moneda;
			FROM bldatos!condicio, bldatos!&tfrom1 RIGHT OUTER JOIN bldatos!&tfrom;
			LEFT OUTER JOIN bldatos!&tfrom_reng;
			LEFT OUTER JOIN art;
			ON  &ttabla_reng.co_art = art.co_art;
			ON  &ttabla.fact_num    = &ttabla_reng.fact_num;
			ON  &ttabla.co_cli      = &ttabla1.co_prov;
			WHERE &ttabla.anulada = .F.;
			AND &ttabla.forma_pag = condicio.co_cond;
			AND &ttabla.fact_num BETWEEN ?tdesde[1] AND ?thasta[1];
			GROUP BY 1,2 ORDER BY 1,2 INTO CURSO &treport

		IF vpar_emp.p_imp_kit
			SELECT *;
				FROM vrepaux;
				INTO CURSO vreportes;
				UNION ALL SELECT &ttabla.fact_num,;
				&ttabla_reng.reng_num,;
				&ttabla5.reng_num AS com_reng,;
				&ttabla.fec_emis,;
				&ttabla5.co_art,;
				art.modelo,art.peso,;
				IIF(es_art_gene(&ttabla_reng.co_art),ALLTRIM(SUBSTR(&ttabla_reng.des_art,1,60)),art.art_des) AS art_des,;
				&ttabla.comentario,;
				&ttabla.fec_venc,;
				&ttabla.co_cli,;
				&ttabla.contrib,;
				&ttabla.DESCRIP,;
				&ttabla1.prov_des AS cli_des,;
				&ttabla1.fax,;
				&ttabla1.telefonos,;
				&NOTA AS nc_num,;
				&ttabla1.rif,;
				&ttabla1.nit,;
				&ttabla1.direc1,;
				&ttabla1.direc2,;
				&ttabla.forma_pag,;
				&ttabla5.uni_venta,;
				&ttabla_reng.co_alma,;
				(&ttabla_reng.total_art*&ttabla5.total_art) AS total_art,;
				&ttabla_reng.tipo_imp, &ttabla_reng.porc_desc,&ttabla_reng.nro_lote,;
				ROUND(&ttabla_reng.prec_vta/IIF(tdivide,1,&ttabla.tasa), 2) AS prec_vta,;
				ROUND(&ttabla_reng.reng_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS reng_neto,;
				&ttabla.porc_gdesc,;
				&ttabla.porc_reca,;
				ROUND(&ttabla.glob_desc/IIF(tdivide,1,&ttabla.tasa), 2) AS glob_desc,;
				ROUND(&ttabla.tot_reca/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_reca,;
				ROUND(&ttabla.iva/IIF(tdivide,1,&ttabla.tasa), 2) AS iva,;
				ROUND(&ttabla.tot_flete/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_flete,;
				&ttabla.dir_ent,&ttabla_reng.comentario AS comentario2,;
				&ttabla_reng.tipo_doc AS rtipo_doc,&ttabla_reng.num_doc AS rnum_doc,;
				ROUND(&ttabla.tot_neto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_neto,;
				ROUND(&ttabla.tot_bruto/IIF(tdivide,1,&ttabla.tasa), 2) AS tot_bruto,;
				condicio.cond_des,;
				condicio.co_cond,;
				&fac AS factura,;
				"COMP" AS tipo,;
				&ttabla1.respons,;
				&ttabla.tot_bruto-&ttabla.glob_desc AS sub_des,;
				&ttabla.tasag,;
				&ttabla.tasa,;
				IIF((rtipo=7 OR rtipo= 9 OR rtipo = 10),LEFT(bus_serial(&ttabla5.co_art,&ttabla.fact_num,IIF(rtipo=9,"S","E"),ttip_doc,0),250), SPACE(1)) AS serial2,;
				&tvuelto AS vuelto,;
				&ttabla.seriales,;
				art.manj_ser,;
				&ttabla.saldo,;
				art.suni_venta,;
				&ttabla_reng.stotal_art,;
				SPACE(30) AS dir_ent2,;
				&ttabla.campo1,;
				&ttabla.campo2,;
				&ttabla.campo3,;
				&ttabla.campo4,;
				&ttabla.campo5,;
				&ttabla.campo6,;
				&ttabla.campo7,;
				&ttabla.campo8,;
				&ttabla.co_sucu, &ttabla.moneda;
				FROM bldatos!&tfrom1, bldatos!condicio, bldatos!&tfrom;
				inner JOIN bldatos!&tfrom_reng;
				inner JOIN bldatos!&tfrom4;
				inner JOIN bldatos!&tfrom5;
				inner JOIN art;
				ON  &ttabla5.co_art  = art.co_art;
				ON  &ttabla4.kit_num      = &ttabla5.kit_num;
				ON  &ttabla_reng.co_art = &ttabla4.co_art;
				ON  &ttabla.fact_num    = &ttabla_reng.fact_num;
				WHERE &ttabla.anulada = .F.;
				AND &ttabla.co_cli    = &ttabla1.co_prov;
				AND &ttabla.forma_pag = condicio.co_cond;
				AND &ttabla.fact_num BETWEEN ?tdesde[1] AND ?thasta[1];
				GROUP BY 1,2,3 ORDER BY 1,2,3
		ENDIF
	 && cÃƒÂ³digo para buscar los seriales de los renglones q tienen seriales asignados	- GAM 190702
	
	 IF (rtipo = 7 OR rtipo = 9 OR rtipo = 10)
		ttipo = IIF(rtipo=9,"S","E")
		tbuscark4  = IIF(ttipo="E",ttip_doc,tinicial)
		tbuscark5  = IIF(ttipo="E",ttip_doc,tfinal)
		tbuscark6  = IIF(ttipo="E",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark7  = IIF(ttipo="E",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))
		tbuscark8  = IIF(ttipo="S",ttip_doc,tinicial)
		tbuscark9  = IIF(ttipo="S",ttip_doc,tfinal)
		tbuscark10 = IIF(ttipo="S",ALLTRIM(STR(tdesde[1])),ALLTRIM(STR(tinicial_doc)))
		tbuscark11 = IIF(ttipo="S",ALLTRIM(STR(thasta[1])),ALLTRIM(STR(tfinal_doc)))

		&& c buscan los seriales asignados al doc
		lca1 = "Seriales.co_art, Seriales.serial, Seriales.doc_num_e "
		lfr1 = "seriales"
		lwh1 = " Seriales.doc_tip_e BETWEEN '"+tbuscark4+"' and '"+tbuscark5+"'"
		lwh2 = " AND Seriales.doc_num_e BETWEEN "+tbuscark6+" AND "+tbuscark7
		lwh3 = " AND Seriales.doc_tip_s BETWEEN '"+tbuscark8+"' and '"+tbuscark9+"'"
		lwh4 = " AND Seriales.doc_num_s BETWEEN "+tbuscark10+" and "+tbuscark11 
		lor1 = " Seriales.doc_num_e, Seriales.co_art, Seriales.contador"
		
		lcadena='select '+lca1+' from '+lfr1+' WHERE '+lwh1+lwh2+lwh3+lwh4+' order by '+lor1+' into cursor vrepauxp'
		&lcadena

	 	lexiste_ser = !(_TALLY=0)
	    && si existen seriales. c busca mediante bus_serial cuales renglones tienen seriales asignados	
	  IF lexiste_ser	
	    && c crea el cursor temporal donde c almacenaran los seriales de c/u renglon		
	    **CREATE CURSOR TEMP_SERIAL (fact_num N(4), reng_num N(4), SERIAL_TEMP M)
		 ** && AD: 09-08-2002  No se porque se creo el fact_num como numerico y no entero preguntar a samper
		CREATE CURSOR TEMP_SERIAL (fact_num I, reng_num I, SERIAL_TEMP M)
		*******************************************************
		SELECT VREPAUXP
		GO TOP
		SELECT VREPORTES.* FROM VREPORTES ORDER BY FACT_NUM, CO_ART INTO CURSOR VREPORTES1
		GO TOP
		&& c recorren los renglones en vreportes1 y c le asigna a c/u su correspondiente serial del  cursor vrepuxup		
		DO WHILE !EOF('VREPORTES1')
		 LSERIAL_ART = ''	
		 IF !EMPTY(alltrim(VREPORTES1.SERIAL2)) AND (VREPORTES1.FACT_NUM=VREPAUXP.DOC_NUM_E) && posee serial
		  LCANT_SACT = VREPORTES1.TOTAL_ART  && c agregan los seriales segun total_art
		  FOR I=1 TO LCANT_SACT
		   LSERIAL_ART = LSERIAL_ART+ALLTRIM(vrepauxp.SERIAL)+' '
		   IF !EOF('VREPAUXP')
		    SKIP 1 IN vrepauxp
		   ENDIF 
		  NEXT
		  SELECT TEMP_SERIAL
		  APPEND BLAN       && c agrega el nro del renglon y el serial
		  REPLACE SERIAL_TEMP WITH alltrim(LSERIAL_ART),;
		  		  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		 ELSE  && no posee serial
		  SELECT TEMP_SERIAL
		  APPEND BLAN
		  REPLACE SERIAL_TEMP WITH '',;
		  		  fact_num WITH VREPORTES1.fact_num,;
		  		  reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
		  && c mueve en vrepauxp hasta el proximo renglon del otro articulo
		  IF VREPORTES1.CO_ART == VREPAUXP.CO_ART  		  
		   SKIP VREPORTES1.TOTAL_ART IN VREPAUXP		  		  
		  ENDIF 
		 ENDIF
		 SKIP IN VREPORTES1
		ENDDO
		&& c efectua el enlace entre vreportes y el cursor temporal de seriales
		SELECT vreportes.*,TEMP_SERIAL.SERIAL_TEMP AS SERIAL FROM VREPORTES,TEMP_SERIAL ;
		 WHERE VREPORTES.fact_num=TEMP_SERIAL.fact_num AND VREPORTES.reng_num=TEMP_SERIAL.reng_num ORDER BY 1,2,3 INTO CURSO VREPORTES
		
	  ELSE  && no existe seriales
	    SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	  ENDIF	 
	 	 
	 ELSE
	  SELECT vreportes.*,;
			SPACE(1) AS serial;
			FROM vreportes;
			ORDER BY 1,2,3;
			INTO CURSOR vreportes	
	 ENDIF	
	&& fin dÃƒÂ³digo de manejo seriales	
	
	ENDIF
ENDIF

**vertabla()

****************************************************************************************
********** Codigo que no permite imprimir factura que no esten canceladas y
********** que no asignaron seriales
***********************************************************************************************
*!*	tnoimpri = .F.
*!*	DO WHILE !EOF()
*!*		IF vreportes.manj_ser AND EMPTY(vreportes.serial)
*!*			tnoimpri = .T.
*!*		ENDIF
*!*		SKIP IN vreportes
*!*	ENDDO
*!*	GO TOP
*!*	SELECT vreportes
*!*	SET FILTER TO IIF(vreportes.co_cond = "001",vreportes.saldo = 0 AND !tnoimpri,.T. AND !tnoimpri)
*!*	GO TOP
llcontinue = .T.


IF !mensaje_report('vreportes')
	llcontinue =  .F.
ENDIF
*********JNP 21/03/2007 DETERMINAR BASES IMPONIBLES E IMPUESTOS POR CADA ALICUOTA
********* se debe utilizar ahora el campo art_1 para indicar la descripcion del articulo
********* para que muestre la (E) en los articulos exentos
SELECT v.fact_num,v.tipo_imp,buscarISV(v.tipo_imp,v.fec_emis,IIF(rtipo <= 6  OR rtipo = 12,"V","C"))+tcero AS tasa_iva, ;
	SUM(reng_neto) AS TOTAL ;
	FROM vreportes v WHERE com_reng =0 ORDER BY fact_num,tipo_imp DESC GROUP BY fact_num,tipo_imp INTO CURSOR valicuota

CREATE CURSOR VREPORTES1 (fact_num1 N(12),tot_bas1 N(18,2),alic1 N(5,2),tot_bas2 N(18,2),alic2 N(5,2),tot_bas3 N(18,2),alic3 N(5,2),tot_bas4 N(18,2),alic4 N(5,2))

SELECT valicuota
SCAN
	SELECT fact_num1 FROM VREPORTES1 WHERE fact_num1=valicuota.fact_num INTO CURSOR vtempo
	SELECT VREPORTES1
	IF EOF('vtempo')
		APPEND BLANK
		REPLACE fact_num1 WITH valicuota.fact_num
	ENDIF
	DO CASE
	CASE tot_bas1=0 AND alic1=0
	    &&REPLACE tot_bas2 WITH valicuota.TOTAL*((100 - porc_comp(vreportes.porc_gdesc,"-") + porc_comp(vreportes.porc_reca,"+"))/100)
		&&REPLACE alic2 WITH valicuota.tasa_iva          
	    REPLACE tot_bas1 WITH valicuota.TOTAL - ROUND(valicuota.TOTAL *((porc_comp(vreportes.porc_gdesc,"-") + porc_comp(vreportes.porc_reca,"+"))/100),2)
	   	REPLACE alic1 WITH valicuota.tasa_iva      &&A.R. 30/10/2008 Se cambio la formula porq el redondeo no se hacia correctamente redondeo. 
    CASE tot_bas2=0 AND alic2=0
		REPLACE tot_bas2 WITH valicuota.TOTAL - ROUND(valicuota.TOTAL *((porc_comp(vreportes.porc_gdesc,"-") + porc_comp(vreportes.porc_reca,"+"))/100),2)
		REPLACE alic2 WITH valicuota.tasa_iva
	CASE tot_bas3=0 AND alic3=0
		REPLACE tot_bas3 WITH valicuota.TOTAL - ROUND(valicuota.TOTAL *((porc_comp(vreportes.porc_gdesc,"-") + porc_comp(vreportes.porc_reca,"+"))/100),2)
		REPLACE alic3 WITH valicuota.tasa_iva
	CASE tot_bas4=0 AND alic4=0
		REPLACE tot_bas4 WITH valicuota.TOTAL - ROUND(valicuota.TOTAL *((porc_comp(vreportes.porc_gdesc,"-") + porc_comp(vreportes.porc_reca,"+"))/100),2)
		REPLACE alic4 WITH valicuota.tasa_iva
	ENDCASE
ENDSCAN

SELECT v.* , ALLTRIM(v.art_des)+IIF(v.tipo_imp='6'," (E)",SPACE(124)) AS art_1,;
	w.* FROM vreportes v, VREPORTES1 w ;
	WHERE v.fact_num=w.fact_num1 INTO CURSOR vreportes
******************* FIN JNP 21/03/07


*!*	***********************************
*!*	* Marcar las facturas como impresas

IF rtipo = 0 OR rtipo = 1 OR rtipo = 3 OR rtipo = 4 OR rtipo = 5 OR rtipo = 6 && Tipo factura, nota de entrega y despacho, pedido y dev de cli
	lresultado = marca_fact_imp(.F.,ttip_doc)
	IF lresultado <> 0
		=MESSAGEBOX(IIF(rtipo=6,"El ","La ")+SUBSTR(alltrim(tnro),1,LEN(alltrim(tnro))-1)+" NÃ‚Âº '" + ALLTRIM(STR(lresultado)) + "' ya fue impres"+IIF(rtipo=6,"o.","a."),16,business_loc)
		llcontinue = .F.
	ENDIF
ENDIF
* Fin Marcar las facturas como impresas
***************************************

select vreportes.* FROM VREPORTES ORDER BY vreportes.co_art INTO CURSOR VREPORTES

canBultos=INT(VAL(ALLTRIM(vreportes.comentario)))
fact_num2=vreportes.fact_num
cli_des2=vreportes.cli_des
reng_neto2=vreportes.reng_neto
dir_ent22=vreportes.dir_ent2
fact_num2=vreportes.fact_num
rnum_doc2=vreportes.rnum_doc
tipo2=vreportes.tipo

CREATE CURSOR PIVOT (numBulto I, totalBultos I, fact_num N(8), reng_neto N(8), dir_ent2 C(100), cli_des C(100), rnum_doc I, tipo C(6))
iterator=0

DO WHILE iterator<canBultos
	iterator=iterator+1
	SELECT PIVOT
	APPEND BLAN 
	REPLACE numBulto WITH iterator,;
			totalBultos WITH canBultos,;
			fact_num WITH fact_num2,;
			reng_neto WITH reng_neto2,;
			dir_ent2 WITH dir_ent22,;
			cli_des WITH cli_des2,;
			rnum_doc WITH rnum_doc2,;
			tipo WITH tipo2
ENDDO

SELECT * FROM PIVOT INTO CURSOR VREPORTES
		*reng_num WITH VREPORTES1.reng_num IN TEMP_SERIAL
RETURN llcontinue
ENDPROC
