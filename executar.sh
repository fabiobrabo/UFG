#!/bin/bash

function executar() {
for i in "$@"
do
echo "Entrou no FTP"
cd /home/ubuntu/arquivos/
ncftpget ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/RD${i}${28}*.dbc


echo "Transforma os arquivo .dbc em csv" 
docker run -it -v /home/ubuntu/arquivos/:/usr/src/app/data dbc2csv make

echo "Exclui *dbf"
cd /home/ubuntu/arquivos/
rm RD${i}${28}*.dbf

cd /home/ubuntu/arquivos/csv/
chmod 777 *

echo "Carrega no Banco de Dados"
for f in /home/ubuntu/arquivos/csv/RD${i}${28}*.csv
do
	mysql -uroot -pfog1717 ufg -e "LOAD DATA LOCAL INFILE '"$f"'
	INTO TABLE datasus
     	FIELDS TERMINATED BY ','
	ENCLOSED BY '\"'
	LINES TERMINATED BY '\n'
	IGNORE 1 LINES" 
	echo "Done: '"$f"' at $(date)"
done

echo "Apaga dos arquivos *.dbc"
cd /home/ubuntu/arquivos/
rm *

echo "Apaga dos arquivos *.csv"
cd /home/ubuntu/arquivos/csv/
rm *

mysql -uroot -pfog1717 ufg -e "create table vw_datasus_${i}
SELECT
MUNIC_RES,
DIAG_PRINC,
COUNT(MUNIC_RES) AS QTD
FROM datasus
WHERE DIAG_PRINC IN ( 'B56' ,'B560' ,'B561' ,'B569' ,'B57' , 'B570', 'B571', 'B572', 'B573', 'B574', 'B575','A90' , 'A91','B72','B67','B670','B671','B672','B673','B674','B675','B676','B677','B678','B679','B55', 'B550', 'B551', 'B552', 'B559','A30', 'A300', 'A301', 'A302', 'A303', 'A304', 'A305', 'A306', 'A307', 'A308', 'A309', 'B92','B350','B351','B352','B353','B354','B355','B356','B357','B358','B359','B360','B361','B362','B363','B364','B365','B366','B367','B368','B369','B370','B371','B372','B373','B374','B375','B376','B377','B378','B379','B380','B381','B382','B383','B384','B385','B386','B387','B388','B389','B390','B391','B392','B393','B394','B395','B396','B397','B398','B399','B400','B401','B402','B403','B404','B405','B406','B407','B408','B409','B410','B411','B412','B413','B414','B415','B416','B417','B418','B419','B420','B421','B422','B423','B424','B425','B426','B427','B428','B429','B430','B431','B432','B433','B434','B435','B436','B437','B438','B439','B440','B441','B442','B443','B444','B445','B446','B447','B448','B449','B450','B451','B452','B453','B454','B455','B456','B457','B458','B459','B460','B461','B462','B463','B464','B465','B466','B467','B468','B469','B470','B471','B472','B473','B474','B475','B476','B477','B478','B479','B480','B481','B482','B483','B484','B485','B486','B487','B488','B489','B490','B491','B492','B493','B494','B495','B496','B497','B498','B499','B35','B36','B37','B38','B39','B40','B41','B42','B43','B44','B45','B46','B47','B48','B49',
'B73','A82','A820','A821','A829','B65','B650','B651','B652','B653','B658','B659','A71' , 'A710' , 'A711' , 'A719','A27','A270','A278','A279','B68','B680','B681','B689','B69','B690','B691','B698','B699','B70','B700','B701','B71','B710','B711','B718','B719','B75','B77','B770','B778','B779','B78','B780','B781','B787','B789','B79','B80','B81','B810','B811','B812','B813','B814','B818','B82','B820','B829','B83','B830','B831','B832','B833','B834','B838','B839',
'B76','B760','B761','B768','B769' )
GROUP BY
MUNIC_RES,
DIAG_PRINC"

mysql -uroot -pfog1717 ufg -e "truncate datasus"
done
}
executar AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO 10
