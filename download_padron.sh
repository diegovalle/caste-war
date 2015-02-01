mkdir INE
curl -C - -o INE/1_PAN.xlsx http://www.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DEPPP/DEPPP-Transparencia/Padron_afiliados_PP/1_PAN.xlsx
curl -C - -o INE/pri.zip http://www.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DEPPP/DEPPP-Transparencia/Padron_afiliados_PP/PRI.zip
curl -C - -o INE/prd.zip http://www.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DEPPP/DEPPP-Transparencia/Padron_afiliados_PP/PRD.zip
#for z in "INE/*.zip"; do unzip -f "$z"; done
find -iname '*.zip' -execdir unzip "{}" \;
find -iname '*.xlsx' -execdir ssconvert "{}" "{}.csv" \;
