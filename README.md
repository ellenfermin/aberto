# Jaguar Dashboard  
### Monitoramento de desmatamento, densidade de onças-pintadas e jaguatiricas e estimativa de mortalidade - Amazon Biome  

Este projeto é um painel interativo desenvolvido em **R Shiny** para integrar informações de **desmatamento**, **densidade potencial de onças-pintadas e jaguatiricas** e **estimativas de mortalidade anual**, utilizando dados espaciais em alta resolução.  
O objetivo é oferecer uma ferramenta rápida, leve e atualizada para apoiar **pesquisa aplicada, conservação, tomada de decisão e apresentações institucionais**.

---

##  Principais Funcionalidades

- **Mapas interativos** com densidade potencial de jaguars de jaguatirica (N / 100 km²)  
- **Integração automática com WFS** para atualizar semanalmente os dados de desmatamento  
- **Pipeline de dados otimizado**: mantém histórico completo e adiciona apenas novas observações  
- **Camadas geográficas comprimidas (.qs)** para carregamento rápido  
- **Filtros dinâmicos por ano** com processamento eficiente  
- **Estimativa anual de onças-pintadas perdidas** com base em regressão espacial  
- **Interface leve e responsiva**, mesmo trabalhando com dados > 3 GB  
- Pensado para uso em **apresentações**, **briefings** e **decisões estratégicas**

---

##  Dados Utilizados

- **Desmatamento (WFS atualizado diariamente)**  
  - Integração automática com serviços remotos  
  - Dados históricos preservados e acrescidos incrementalmente

- **Distribuição Potencial de Onças-Pintadas (Raster 1 km²)**  
  - Baseada em regressão múltipla:  
    - TEMP  
    - NPPmean  
    - NPPsd  
    - Continente

- **Limites Municipais e Áreas Protegidas (.qs)**  
  - Compressão com o pacote `{qs}` para otimizar carregamento no Shiny

---

## 🔧 Tecnologias e Pacotes

**Back-end & Processamento**
- R  
- `dplyr`, `sf`, `terra`, `purrr`, `qs`, `fasterize`

**Dashboard**
- `shiny`, `shinydashboard`, `leaflet`, `plotly`, `DT`

**Automação**
- Scripts agendados 
- Atualização a cada 10 dias dos dados WFS  
- Pipeline incremental para manter histórico completo

---

## 📁 Estrutura do Projeto

