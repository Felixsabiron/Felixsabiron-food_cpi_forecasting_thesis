library(shiny)
library(ggplot2)
library(gt)
library(dplyr)

# Données ---------

# À placer juste avant ui <- navbarPage(...)
modeles_disponibles <- c(
 "ARMA", "AR(1)", "AR(P)", "X13", "STL", "HoltWinters",
 "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA",
 "Désagrégée", "Random forest", "ARX", "Naïve", "Observée"
)

# Statistiques descriptives pour le prix du pétrole
stat_desc_petrole <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  18.5, 124.5, 57.45, 103.575, 78.13631,
  75.65, 13126.9, 1.962406, 74.261989,
  82.010630, 646.974063, 25.435685, 0.027964, -1.107434
 )
)

# Data frame pour le taux de change EUR/USD
stat_desc_tauxchange <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  0.690000, 1.020000, 0.770000, 0.900000, 0.842262,
  0.855000, 141.500000, 0.005995, 0.830426,
  0.854098, 0.006038, 0.077704, -0.182187, -1.033366
 )
)

stat_desc_combustibles <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  2064.000, 15846.000, 4749.750, 7475.750, 6323.500,
  5746.000, 1062348.000, 195.3747, 5937.778,
  6709.222, 6412771.000, 2532.345, 1.434538, 2.739867
 )
)

# Création du data.frame pour Prix Mondial du Gaz
stat_desc_gaz <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  1.462612, 69.977239, 5.886890, 11.38, 10.836746,
  8.867352, 1820.573340, 0.731233, 9.393094,
  12.280398, 89.829889, 9.477863, 3.225024, 12.953485
 )
)

# Création du data.frame pour FAO Food Index
stat_desc_fao <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  84.934277, 160.224455, 95.872983, 124.258742, 111.641130,
  111.061659, 18755.709803, 1.332932, 109.009562,
  114.272698, 298.486676, 17.276767, 0.461513, -0.645481
 )
)

stat_desc_inflation <- data.frame(
 Statistique = c(
  "Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
  "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
  "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"
 ),
 Valeur = c(
  216.687, 307.789, 233.064, 258.78375, 249.880518,
  242.284, 41979.927, 1.881786, 246.165363,
  253.595673, 594.907717, 24.39073, 0.951121, 0.004542
 )
)

# Statistiques descriptives pour le GPR
stat_desc_gpr <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(0.19, 2.80, 0.36, 0.64, 0.564, 0.51, 94.77,
            0.0261, 0.513, 0.616, 0.1144, 0.3383, 3.1841, 14.3975)
)

stat_desc_climat <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(0.6, 2.52, 1.14, 1.53, 1.365, 1.31, 229.35,
            0.0288, 1.308, 1.422, 0.1395, 0.3735, 0.7094, 0.467)
)

stat_desc_ippap <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(82.0, 149.6, 100.9, 110.1, 109.5, 106.0, 18286.5,
            1.146, 107.237, 111.763, 219.390, 14.812, 1.257, 1.093)
)

stat_desc_confiance_menages <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(79.867, 109.218, 86.701, 98.828, 92.848, 92.573, 15505.543,
            0.557, 91.748, 93.947, 51.788, 7.196, 0.159, -1.137)
)

stat_desc_conso_alim <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(15837.58, 18606.34, 16970.99, 17380.60, 17155.32, 17174.82, 2864939.0,
            32.732, 17090.70, 17219.95, 178922.9, 422.993, -0.557, 1.858)
)

stat_desc_cout_horaire <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(103.2, 136.8, 114.5, 122.75, 117.857, 115.1, 19682.2,
            0.537, 116.796, 118.919, 48.233, 6.945, 0.629, 0.236)
)

stat_desc_ipp <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(86.7, 131.0, 91.3, 94.5, 97.193, 93.0, 16231.2, 0.904, 95.407, 98.979,
            136.620, 11.688, 1.847, 1.870)
)

# Création du tableau des statistiques descriptives SANS les deux lignes du test
stat_desc_df <- data.frame(
 Statistique = c("Minimum", "Maximum", "1er Quartile", "3e Quartile", "Moyenne",
                 "Médiane", "Somme", "SE Moyenne", "IC Inférieure Moyenne", 
                 "IC Supérieure Moyenne", "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
 Valeur = c(0, 4.5, 0, 1, 0.599, 0.05, 100.1, 0.081, 0.439, 0.76, 1.1, 1.049, 2.392, 5.358)
)

ts_test01 <- window(ts1IPC01_CVS_RJDmetra_corr_diff, start = c(2023, 1), end = c(2023, 12))

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
 df <- data.frame(
  date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
  value = as.numeric(fcast_obj$mean),
  model = name
 )
 df %>% slice_head(n = 12)
}

ts_forex13_01 <- ts(as.numeric(forex13_01), start = c(2023, 1), frequency = 12)

# 2. Construire un objet forecast manuellement (sans recalculer)
forecast_x13_01 <- structure(list(
 mean = ts_forex13_01,  # ici, un vrai ts avec start/end
 lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
 level = c(80, 95),
 x = NULL,
 method = "X13",
 fitted = NULL,
 residuals = NULL
), class = "forecast")

prev_rf$mean <- ts(prev_rf$mean, start = c(2023, 1), frequency = 12)

dfs_01 <- list(
 extract_forecast_df(prev_arima, "ARMA"),
 extract_forecast_df(prev_ar1, "AR(1)"),
 extract_forecast_df(prev_arP, "AR(P)"),
 extract_forecast_df(forecast_x13_01, "X13"),
 extract_forecast_df(prevstl, "STL"),
 extract_forecast_df(forecast_hw, "HoltWinters"),
 extract_forecast_df(prev_ADAM_ETS, "ADAM_ETS"),
 extract_forecast_df(prev_AES, "ADAM_ETS+SARIMA"),
 extract_forecast_df(prev_SSARIMA, "SSARIMA"),
 extract_forecast_df(pred_ipc_disagg, "Désagrégée"),
 extract_forecast_df(prev_rf, "Random forest"),
 extract_forecast_df(pred_naive, "Naïve"),
 extract_forecast_df(forecast_armax, "ARX"))

#Je vais comparer aveclesvaleur reel grace a ts_test01
observed_df_01 <- data.frame(
 date = as.Date(as.yearmon(time(ts_test01))),
 value = as.numeric(ts_test01),
 model = "Observée")

# Combine toutes les prévisions et l'observée
df_all_01 <- bind_rows(dfs_01, observed_df_01)
df_all_01$date <- as.Date(df_all_01$date, format = "%Y-%m-%d")
library(ggplot2)

# Ton graphique tel que tu l’as écrit — inchangé
pA <- ggplot(df_all_01, aes(x = date, y = value, color = model, linetype = model)) +
 geom_line(size = 1.2) +
 labs(
  title = "Prévisions - Sur 12 mois (2023)- methdode désagrégée",
  x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
 ) +
 scale_color_manual(values = c(
  "Désagrégée" = "darkred",
  "Observée" = "blue"
 )) +
 scale_linetype_manual(values = c(
  "Désagrégée" = "solid",
  "Observée" = "solid"
 )) +
 scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
 theme_minimal(base_size = 13) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )

# Données tableau
data <- data.frame(
 Code_COICOP = c("01", "01.1", "01.1.1", "01.1.2", "01.1.3", "01.1.4", "01.1.5", 
                 "01.1.6", "01.1.7", "01.1.8", "01.1.9", "01.2", "01.2.1", "01.2.2"),
 Categorie = c(
  "  Produits alimentaires et boissons non alcoolisées", 
  "  Produits alimentaires",
  "    Pain et céréales", 
  "    Viande", 
  "    Poissons et fruits de mer", 
  "    Lait, fromage et œufs", 
  "    Huiles et graisses", 
  "    Fruits", 
  "    Légumes", 
  "    Sucre, confiture, miel, chocolat et confiserie", 
  "    Produits alimentaires n.c.a.",
  "  Boissons non alcoolisées",
  "    Café, thé et cacao", 
  "    Eaux minérales, boissons rafraîchissantes, jus de fruits et de légumes"
 ),
 Ponderation = c(100.00, NA , 21.82, 18.92, 4.50, 13.04, 2.36, 6.56, 10.83, 7.78, 4.96, NA, 3.51, 2.21),
 stringsAsFactors = FALSE
)

# Fonction GT
create_gt_table <- function(df) {
 df %>%
  gt() %>%
  tab_header(
   title = md("**Tableau – Pondérations des sous-catégories principales en 2023 (en \\%)**")
  ) %>%
  fmt_number(columns = c("Ponderation"), decimals = 2) %>%
  fmt_missing(columns = c("Ponderation"), missing_text = "") %>%
  cols_label(
   Code_COICOP = "Code COICOP",
   Categorie = "Catégorie",
   Ponderation = "Pondération (%)"
  ) %>%
  cols_align(align = "left", columns = c("Categorie")) %>%
  tab_style(
   style = cell_text(weight = "bold"),
   locations = cells_body(rows = df$Code_COICOP %in% c("01", "01.1", "01.2"))
  ) %>%
  tab_options(
   table.border.top.width = px(2),
   table.border.bottom.width = px(2),
   table.border.top.color = "black",
   table.border.bottom.color = "black",
   heading.border.bottom.color = "black",
   column_labels.border.bottom.width = px(1),
   column_labels.border.bottom.color = "black",
   table_body.border.bottom.color = "black"
  )
}





### UI ------
# UI
ui <- navbarPage(
 title = "M1 ECAP - Mémoire",
 
 # Partie 1 acceuil ---- 
 tabPanel("Accueil",
          fluidPage(
           div(style = "text-align:center; margin-top: 50px;",
               h1("M1 ECAP", style = "font-weight:bold; font-size: 48px;"),
               h3("MEMOIRE", style = "font-size: 32px;"),
               h4("Prévision désagrégée de l'IPC", style = "font-size: 32px;"),
               tags$div(style = "height: 50px;")
           ),
           
           # ✅ Texte de présentation
           div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
               HTML("
          <p><strong>Ce site présente les méthodes et les résultats issus du travail de recherche mené par Félix Sabiron</strong> dans le cadre de son mémoire de Master 1 ECAP.</p>

          <p>L’étude porte sur la prévision de l’indice des prix à la consommation (IPC) du secteur <em>“alimentation et boissons non alcoolisées”</em> en France, un indicateur clé du suivi de l’inflation.</p>

          <p><strong>Deux approches sont comparées :</strong></p>
          <ul>
            <li><strong>Approche directe :</strong> modélisation de l’IPC global à l’aide de différents modèles statistiques.</li>
            <li><strong>Approche désagrégée :</strong> modélisation séparée des 11 composantes de l’indice, avant agrégation des prévisions selon les pondérations officielles de l’INSEE.</li>
          </ul>

          <h4>Structure du site</h4>
          <ul>
            <li><strong>Accueil :</strong> Présentation du contexte, des objectifs et de la méthodologie.</li>
            <li><strong>Agrégé :</strong> Résultats des modèles appliqués directement à l’IPC global.</li>
            <li><strong>Composantes :</strong> Résultats détaillés des 10 modèles testés sur chacune des 11 composantes, avec sélection du meilleur modèle selon des critères statistiques (DM-test, MAE, RMSE…).</li>
            <li><strong>Variables :</strong> Présentation des variables utilisées dans les modèles Random Forest et ARX, avec transformations, tests de stationnarité et statistiques descriptives.</li>
            <li><strong>Rowlin :</strong> Section de prévision en fenêtre roulante (rolling forecast), avec agrégation mensuelle des prévisions.</li>
          </ul>
        ")
           ),
           tags$div(style = "height: 50px;"),
           
           withMathJax(
            div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                tags$div(style = "border: 1px solid #ccc; border-radius: 6px; box-shadow: 2px 2px 4px rgba(0,0,0,0.1);",
                         tags$div(style = "background-color: #444; color: white; padding: 10px; border-top-left-radius: 6px; border-top-right-radius: 6px;",
                                  tags$strong("Méthodologie")
                         ),
                         tags$div(style = "background-color: #f9f9f9; padding: 20px;",
                                  HTML("
                          <p>La prévision agrégée est obtenue par :</p>
                          <div style='text-align: center; font-size: 20px; margin: 20px 0;'>
                            $$ \\hat{\\pi}_t = \\sum_{i=1}^{11} w_i \\hat{y}_{i,t} $$
                          </div>
                          <p>où <em>w<sub>i</sub></em> est le poids de la composante <em>i</em>, et <em>\\( \\hat{y}_{i,t} \\)</em> sa valeur prédite.</p>
                        ")
                         )
                )
            )
           ),
           
           tags$div(style = "height: 50px;"),
           
           
           
           div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
               HTML("
      <p> Le graphique ci-dessous compare les prévisions agrégées (obtenues à partir des composantes modélisées séparément) aux valeurs réelles observées. Il permet de visualiser les résultats obtenus avec cette méthode.</p>

      <p>En complément, le tableau affiche la liste des 11 composantes de l’IPC alimentaire utilisées dans l’approche désagrégée, accompagnées de leur pondération officielle dans l’indice global. Ces pondérations, fournies par l’INSEE, ont servi à agréger les prévisions individuelles en une estimation unique de l’IPC alimentaire.</p>
    ")
           ),
           tags$div(style = "height: 50px;"),
           
           
           # Conteneur commun au graphique et au tableau
           div(style = "margin: auto; width: 50%;",
               plotOutput("graph_p1", height = "500px"),
               tags$div(style = "height: 100px;"),
               gt_output("table_gt")
           ),
           
           tags$div(style = "height: 100px;")
          )
 ),
 
 # PArite 2 agrée ---- 
 tabPanel("Agrégé",
          titlePanel("Prévisions de la série agrégée à l’aide des modèles univariés et multivariés"),
          
          ## SECTION A ----
          h2("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
          
          radioButtons("graph_choice", "Transformation de la série agréée :",
                       choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr_diff"),
                       inline = TRUE,
                       width = "100%"),
          
          tags$div(style = "height: 30px;"),
          
          # Graphique A centré dans la page et encadré proprement
          fluidRow(
           column(width = 2),
           column(width = 8,
                  div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px; padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                      plotOutput("graph_selected", height = "350px"))
           ),
           column(width = 2)
          ),
          
          tags$div(style = "height: 50px;"),
          
          div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
              HTML("
      <p>La série de l’IPC 01 a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
          ),
          
          tags$div(style = "height: 50px;"),
          
          ## SECTION B ----
          h2("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
          
          fluidRow(
           column(
            width = 8,
            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px; padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                plotOutput("graph_previsions", height = "500px"))
           ),
           column(
            width = 4,
            div(style = "margin-left: 10px; padding-top: 20px;",
                checkboxInput("select_all", "Tous les modèles", value = FALSE),
                checkboxGroupInput("modeles_selectionnes", 
                                   "Choisir les modèles à afficher :", 
                                   choices = modeles_disponibles,
                                   selected = c("Observée", "ARMA", "Désagrégée"))
            )
           )
          ),
          
          tags$div(style = "height: 60px;"),
          div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
              HTML("
      <p>Le graphique de comparaison des prévisions permet de visualiser les résultats obtenus par chacun des modèles, en les confrontant aux valeurs réelles ainsi qu’aux prévisions issues de la méthode désagrégée.</p>
    ")
          ),
          tags$div(style = "height: 60px;"),
          
          ## SECTION C ----
          h2("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
          
          tags$div(style = "height: 20px;"),
          
          
          div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
              HTML("
      <p>Le graphique des erreurs cumulées (<em>CSPE</em>), ainsi que les tableaux de comparaison des performances des modèles (en termes de <strong>MSE</strong> et de <strong>R²</strong> hors échantillon), et les résultats du test de <strong>Diebold-Mariano</strong>, mettent en évidence la supériorité de la méthode désagrégée en termes de précision prédictive.</p>
    ")
          ),
          tags$div(style = "height: 20px;"),
          
          
          # ---- CSPE block 
          fluidRow(
           column(width = 8,
                  div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
                     padding: 20px; margin-top: 10px; margin-bottom: 30px;
                     box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                      h4("Graphique CSPE : Erreurs cumulées", style = "margin-bottom: 20px; font-weight: bold;"),
                      plotOutput("graph_cspe", height = "400px"))
           ),
           column(width = 4,
                  div(style = "margin-top: 50px; margin-left: 10px;",
                      checkboxGroupInput("cspe_modeles",
                                         "Afficher les modèles dans le graphique CSPE :",
                                         choices = unique(cspe_df_long$Model),
                                         selected = c("Naïve", "Désagrégée", "ARMA"))
                  )
           )
          ),
          # escpace
          tags$div(style = "height: 40px;"),
          
          # ---- TABLEAUX COMPARATIFS
          fluidRow(
           column(width = 6,
                  div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                     box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                      h4("Comparaison des performances des modèles", style = "font-weight: bold; margin-bottom: 20px;"),
                      gt_output("table_performance")
                  )
           ),
           column(width = 6,
                  div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                     box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                      h4("Test de Diebold-Mariano", style = "font-weight: bold; margin-bottom: 20px;"),
                      gt_output("table_dm")
                  )
           )
          ),
          
          
          ## SECTION D ----
          h2("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
          
          # D.1 : Tests statistiques
          h3("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
          
          div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
              HTML("
      <p>Les données de la série étudiée (variable d’intérêt) ont été correctement corrigées de leur saisonnalité et rendues stationnaires.</p>
    ")
          ),
          tags$div(style = "height: 50px;"),
          
          fluidRow(
           column(width = 6,
                  verbatimTextOutput("test_combined"),
                  verbatimTextOutput("test_seasdum")
           ),
           column(width = 6,
                  verbatimTextOutput("test_adf"),
                  verbatimTextOutput("test_kpss")
           )
          ),
          
          tags$div(style = "height: 40px;"),
          
          # D.2 : Outliers
          h3("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
          
          fluidRow(
           column(width = 6,
                  plotOutput("plot_outliers", height = "300px")
           ),
           column(width = 6,
                  verbatimTextOutput("outliers_resume")
           )
          ),
          
          tags$div(style = "height: 80px;"),
          
          div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
              HTML("
      <p>La série a révélé la présence de plusieurs valeurs atypiques. À l’aide du package <code>tso</code>, quatre points atypiques de type <em>Level Shift</em> (changement de niveau soudain) ont été détectés, situés entre 2015 et 2022.</p>
    ")
          ),
          
          
          
          # ---- ESPACE FINAL
          tags$div(style = "height: 80px;")
 ),
 
 ### Partie sur les sections : ----
 tabPanel("Comp",
          h1("Estimation d’un modèle univarié optimal pour chaque composante", style = "text-align: center; margin-top: 30px;"),
          
          tabsetPanel(
           # Comp 1  ----
           tabPanel("1",
                    h2("01.1.1 – Pain et céréales", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_1", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_1", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 30px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.1</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    
                    
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.1)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_1", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_1", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_1", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_1$model),
                                                   selected = c("Observée", "AR(1)", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_1)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_1)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>AR(1)</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.1</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_1"),
                            verbatimTextOutput("test_seasdum_1_1")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_1"),
                            verbatimTextOutput("test_kpss_1_1")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            plotOutput("plot_outliers_1_1", height = "300px")
                     ),
                     column(width = 6,
                            verbatimTextOutput("outliers_resume_1_1")
                     )
                    ),
                    tags$div(style = "height: 40px;"),
                    
                    
           ),
           
           
           # Comp 2  ----
           tabPanel("2",
                    h2("01.1.2 Viande", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_2", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_2", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.2</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.2)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_2", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_2", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_2", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_2$model),
                                                   selected = c("Observée", "SSARIMA", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_2)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_2")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_2)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_2")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Pour la série <strong>IPC 01.1.2</strong>, le modèle <strong>SSARIMA</strong> a été retenu.</p>
      
      <p>Bien que le modèle <strong>AR(1)</strong> présente de bonnes performances, le <strong>SSARIMA</strong> offre un meilleur compromis global entre les différents critères d’évaluation.</p>
      
      <p>Ses prévisions seront donc utilisées dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_2"),
                            verbatimTextOutput("test_seasdum_1_2")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_2"),
                            verbatimTextOutput("test_kpss_1_2")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            plotOutput("plot_outliers_1_2", height = "300px")
                     ),
                     column(width = 6,
                            verbatimTextOutput("outliers_resume_1_2")
                     )
                    ),
                    tags$div(style = "height: 40px;"),
                    
                    
           ),
           
           # Comp 3  ----
           tabPanel("3",
                    h2("01.1.3 Poissons et fruits de mer", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_3", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_3", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.3</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.3)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_3", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_3", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_3", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_3$model),
                                                   selected = c("Observée", "SSARIMA", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_3)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_3")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_3)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_3")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Pour la série <strong>IPC 01.1.3</strong>, le modèle <strong>SSARIMA</strong> a été retenu.</p>
      
      <p>Bien que le modèle <strong>AR(1)</strong> présente de bonnes performances, le <strong>SSARIMA </strong> offre un meilleur compromis global entre les différents critères d’évaluation.</p>
      
      <p>Ses prévisions seront donc utilisées dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_3"),
                            verbatimTextOutput("test_seasdum_1_3")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_3"),
                            verbatimTextOutput("test_kpss_1_3")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_3", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_3"))
                    ),
                    tags$div(style = "height: 30px;"),
           ),
           
           # Comp 4  ----
           tabPanel("4",
                    h2("01.1.4 Lait, fromage et œufs", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_4", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_4", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.4</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.4)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_4", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_4", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_4", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_4$model),
                                                   selected = c("Observée", "HoltWinters", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_4)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_4")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_4)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_4")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>Holt-Winters</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.4</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_4"),
                            verbatimTextOutput("test_seasdum_1_4")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_4"),
                            verbatimTextOutput("test_kpss_1_4")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_4", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_4"))
                    ),
                    tags$div(style = "height: 30px;"),
                    
           ),
           
           # Comp 5  ----
           tabPanel("5",
                    h2("01.1.5 Huiles et graisses", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_5", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_5", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.5</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.5)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_5", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_5", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_5", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_5$model),
                                                   selected = c("Observée", "ADAM_ETS", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_5)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_5")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_5)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_5")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>ADAM_ETS</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.5</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_5"),
                            verbatimTextOutput("test_seasdum_1_5")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_5"),
                            verbatimTextOutput("test_kpss_1_5")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_5", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_5"))
                    ),
                    tags$div(style = "height: 30px;"),
           ),
           
           # Comp 6  ----
           tabPanel("6",
                    h2("01.1.6 Fruits", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_6", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_6", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.6</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.6)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_6", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_6", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_6", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_6$model),
                                                   selected = c("Observée", "SSARIMA", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_6)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_6")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_6)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_6")
                            )
                     )
                    ),
                    tags$div(style = "height: 60px;"),
                    
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>SSARIMA</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.6</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 60px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_6"),
                            verbatimTextOutput("test_seasdum_1_6")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_6"),
                            verbatimTextOutput("test_kpss_1_6")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_6", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_6"))
                    ),
                    tags$div(style = "height: 30px;"),    
                    
           ),
           
           # Comp 7  ----
           tabPanel("7",
                    h2("01.1.7 Légumes", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_7", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_7", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.7</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.7)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_7", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_7", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_7", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_7$model),
                                                   selected = c("Observée", "AR(1)", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_7)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_7")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_7)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_7")
                            )
                     )
                    ),
                    tags$div(style = "height: 60px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La composante <strong>IPC 01.1.7</strong> semble s’ajuster au mieux à un modèle <strong>AR(1)</strong>.</p>
    ")
                    ),
                    tags$div(style = "height: 60px;"),
                    
                    
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_7"),
                            verbatimTextOutput("test_seasdum_1_7")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_7"),
                            verbatimTextOutput("test_kpss_1_7")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_7", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_7"))
                    ),
                    tags$div(style = "height: 30px;"),
                    
           ),
           
           # Comp 8  ----
           tabPanel("8",
                    h2("01.1.8 Sucre, confiture, miel, chocolat et confiserie", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_8", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_8", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.8</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.8)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_8", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_8", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_8", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_8$model),
                                                   selected = c("Observée", "AR(1)", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_8)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_8")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_8)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_8")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>AR(1)</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.8</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_8"),
                            verbatimTextOutput("test_seasdum_1_8")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_8"),
                            verbatimTextOutput("test_kpss_1_8")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_8", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_8"))
                    ),
                    tags$div(style = "height: 30px;"),
           ),
           
           # Comp 9  ----
           tabPanel("9",
                    h2("01.1.9 Produits alimentaires n.c.a.", style = "text-align: center;"), 
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_9", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_9", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.1.9</strong> a été ajustée de ses points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.1.9)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_1_9", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_1_9", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_1_9", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_1_9$model),
                                                   selected = c("Observée", "AR(P)", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_1_9)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_9")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_1_9)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_9")
                            )
                     )
                    ),
                    tags$div(style = "height: 60px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>AR(P)</strong> se distingue comme le plus performant pour la série <strong>IPC 01.1.9</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_1_9"),
                            verbatimTextOutput("test_seasdum_1_9")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_1_9"),
                            verbatimTextOutput("test_kpss_1_9")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_1_9", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_1_9"))
                    ),
                    tags$div(style = "height: 30px;"),
           ),
           
           # Comp 10  ----
           tabPanel("10",
                    h2("01.2.1 Café, thé et cacao", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_10", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_10", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.2.1</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.2.1)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_2_1", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_2_1", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_2_1", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_2_1$model),
                                                   selected = c("Observée", "AR(1)", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_2_1)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_2_1")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_2_1)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_2_1")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>AR(1)</strong> se distingue comme le plus performant pour la série <strong>IPC 01.2.1</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_2_1"),
                            verbatimTextOutput("test_seasdum_2_1")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_2_1"),
                            verbatimTextOutput("test_kpss_2_1")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_2_1", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_2_1"))
                    ),
                    tags$div(style = "height: 30px;"),
           ),
           
           # Comp 11  ----
           tabPanel("11",
                    h2("01.2.2 Eaux minérales, boissons rafraîchissantes, jus", style = "text-align: center;"),
                    h4("A. Graphique de la série étudiée", style = "margin-top: 30px; text-align: center;"),
                    #### serie ----
                    
                    # ⬅️ Radio et graphe de la série
                    tags$div(style = "margin: 30px;",
                             radioButtons("graph_choice_11", "Transformation de la série :",
                                          choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                          inline = TRUE),
                             plotOutput("plot_choisi_11", height = "350px")
                    ),
                    
                    # ⬅️ Espace
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>La série de l’IPC <strong>01.2.2</strong> a été corrigée des variations saisonnières à l’aide de la méthode <strong>X13-ARIMA-SEATS</strong>, puis ajustée pour les points atypiques à l’aide du package <code>tso</code>.</p>
      
      <p>Elle a ensuite été différenciée pour garantir sa stationnarité.</p>
      
      <p>Le graphique ci-dessus illustre les transformations successives appliquées à la série et les changements qu’elles ont induits.</p>
    ")
                    ),
                    tags$div(style = "height: 50px;"),
                    
                    #### prévisions ----
                    h4("B Comparaison des prévisions", style = "text-align: center; margin-top: 30px;"),
                    
                    
                    # ⬅️ Bloc des prévisions interactives
                    fluidRow(
                     column(width = 8,
                            div(style = "text-align: center; border: 1px solid #ccc; border-radius: 8px;
           padding: 20px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Prévisions sur 12 mois par modèle (composante 01.2.2)", 
                                   style = "font-weight: bold; margin-bottom: 20px;"),
                                plotOutput("graph_previsions_1_2_2", height = "450px")
                            )
                     ),
                     column(width = 4,
                            div(style = "margin-left: 10px; padding-top: 20px;",
                                checkboxInput("select_all_1_2_2", "Tous les modèles", value = FALSE),
                                checkboxGroupInput("modeles_selectionnes_1_2_2", 
                                                   "Choisir les modèles à afficher :", 
                                                   choices = unique(df_all_01_2_2$model),
                                                   selected = c("Observée", "X13", "Naïve"))
                            )
                     )
                    ),
                    
                    tags$div(style = "height: 60px;"),
                    
                    #### Qualité des prévisions ----
                    h4("C. Qualité des prévisions", style = "text-align: center; margin-top: 50px;"),
                    
                    
                    
                    fluidRow(
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Comparaison des performances des modèles (01_2_2)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_performance_01_2_2")
                            )
                     ),
                     column(width = 6,
                            div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
                                h4("Test de Diebold-Mariano (01_2_2)", style = "font-weight: bold; margin-bottom: 20px;"),
                                gt_output("table_dm_01_2_2")
                            )
                     )
                    ),
                    tags$div(style = "height: 50px;"),
                    div(style = "width: 75%; margin: auto; font-size: 16px; line-height: 1.6;",
                        HTML("
      <p>Le modèle <strong>X13</strong> se distingue comme le plus performant pour la série <strong>IPC 01.2.2</strong>. Ses prévisions seront donc retenues dans le cadre de la méthode désagrégée.</p>
    ")
                    ),
                    tags$div(style = "height: 70px;"),
                    
                    h4("D. Traitement des données", style = "text-align: center; margin-top: 60px;"),
                    h5("D.1 – Tests de stationnarité et saisonnalité", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    
                    fluidRow(
                     column(width = 6,
                            verbatimTextOutput("test_combined_2_2"),
                            verbatimTextOutput("test_seasdum_2_2")
                     ),
                     column(width = 6,
                            verbatimTextOutput("test_adf_2_2"),
                            verbatimTextOutput("test_kpss_2_2")
                     )
                    ),
                    tags$div(style = "height: 30px;"),
                    
                    h5("D.2 – Valeurs atypiques détectées", style = "margin-left: 20px; margin-top: 30px; font-weight: bold;"),
                    fluidRow(
                     column(width = 6, plotOutput("plot_outliers_01_2_2", height = "300px")),
                     column(width = 6, verbatimTextOutput("outliers_resume_01_2_2"))
                    ),
                    tags$div(style = "height: 30px;"),
                    
           ),
           
          )
 ),
 # Variables ----
 tabPanel("Variables",
          h2("Exploration des variables", style = "text-align:center; margin-bottom: 20px;"),
          
          radioButtons("var_filter", "Afficher :",
                       choices = c("Toutes les variables" = "all",
                                   "Variables exogènes seulement" = "exo"),
                       inline = TRUE
          ),
          
          conditionalPanel(
           condition = "input.var_filter == 'all'",
           tabsetPanel(
            tabPanel("Taux directeur ", 
                     h2("A Taux directeur de la BCE", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 1 --------
                     plotOutput("plot_taux_directeur", height = "400px"),
                     ## SECTION 2
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives", 
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"
                                 ),
                                 gt_output("table_stat_desc")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_taux_directeur", height = "300px", width = "500px")
                             )
                      )
                     ),
                     
            ),
            
            
            tabPanel("IPP", 
                     h2("Indices de prix de production et d’importation", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 2 --------
                     plotOutput("plot_ipp", height = "400px"),
                     ## SECTION 2
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"
                                 ),
                                 gt_output("table_stat_ipp")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_ipp", height = "300px", width = "500px")
                             )
                      )
                     ),
                     
                     ## SECTION 3 
                     h2("C Valeurs atypiques détectées", style = "text-align: center; margin-top: 60px;")
            ),
            
            
            tabPanel("Coût horaire", 
                     h2("Coût horaire du travail", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 3 --------
                     plotOutput("plot_cout_horaire", height = "400px"),
                     ## SECTION 2
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"
                                 ),
                                 gt_output("table_stat_cout")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_cout_horaire", height = "300px", width = "500px")
                             )
                      )
                     ),
                     
            ),
            tabPanel("Confiance des ménages", 
                     h2("Indice de confiance des ménages", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 4 --------
                     plotOutput("plot_confiance_menages", height = "400px"),
                     ## SECTION 2
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"
                                 ),
                                 gt_output("table_stat_confiance")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_confiance", height = "300px", width = "500px")
                             )
                      )
                     ),
            ),
            
            tabPanel("Inflation US", 
                     h2("Indice des prix à la consommation aux États-Unis (IPC)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 5 --------
                     plotOutput("plot_inflation_us", height = "400px"),
                     ## SECTION 2
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"
                                 ),
                                 gt_output("table_stat_inflation_us")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_inflation_us", height = "300px", width = "500px")
                             )
                      )
                     ),
                     
            ),
            
            tabPanel("IPPAP", 
                     h2("Indice des prix agricoles à la production", style = "text-align: center; margin-top: 60px;"),
                     
                     # Graphique principal
                     plotOutput("plot_ippap", height = "400px"),
                     
                     ## SECTION 2 — Titre
                     h2("B Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     
                     ## SECTION 2 — Tableau descriptif centré
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "display: inline-block; margin: auto; 
                       border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                       box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                       text-align: center;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px;"),
                                 gt_output("table_stat_ippap")
                             )
                      )
                     ),
                     
                     ## SECTION 3 — Boxplot
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
                      width: fit-content; margin: auto;",
                                 plotOutput("boxplot_ippap", height = "300px", width = "500px")
                             )
                      )
                     ),
            ),
            
            
            tabPanel("Prix Pétrole", 
                     h2("Prix du pétrole brut (USD/baril)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 7 --------
                     radioButtons("graph_choice_petrole2", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_petrole2", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_petrole_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_petrole2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_petrole2"),
                             verbatimTextOutput("test_seasdum_petrole2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_petrole2"),
                             verbatimTextOutput("test_kpss_petrole2")
                      ),
                     )
                     
            ),
            
            tabPanel("Tx change EUR/USD", 
                     h2("Taux de change EUR/USD", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 8 --------
                     radioButtons("graph_choice_change2", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_change2", height = "350px"),
                     h2("B Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_tauxchange_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_fx2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_change2"),
                             verbatimTextOutput("test_seasdum_change2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_change2"),
                             verbatimTextOutput("test_kpss_change2")
                      )
                     )
                     
            ),
            
            tabPanel("Importation combustibles", 
                     h2("Importations de combustibles (en millions d’euros)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 9 --------
                     radioButtons("graph_choice_import2", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_import2", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_combustibles_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_combustibles2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_combustibles2"),
                             verbatimTextOutput("test_seasdum_combustibles2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_combustibles2"),
                             verbatimTextOutput("test_kpss_combustibles2")
                      )
                     )
                     
            ),
            
            tabPanel("Prix Mondial du Gaz", 
                     h2("Prix mondial du gaz naturel (UE)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 10 --------
                     radioButtons("graph_choice_gaz_2", "Transformation :",
                                  choices = c("Série brute" = "brute", "Corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_gaz_2", height = "350px"),
                     h2("B Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_gaz_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_gaz2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_gaz2"),
                             verbatimTextOutput("test_seasdum_gaz2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_gaz2"),
                             verbatimTextOutput("test_kpss_gaz2")
                      )
                     )
                     
                     
            ),
            
            tabPanel("FAO Food Index", 
                     h2("Indice FAO des prix des produits alimentaires", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 11 --------
                     radioButtons("graph_choice_fao_2", "Transformation :",
                                  choices = c("Série brute" = "brute", "Corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_fao_2", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_fao_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_fao2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_fao2"),
                             verbatimTextOutput("test_seasdum_fao2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_fao2"),
                             verbatimTextOutput("test_kpss_fao2")
                      ),
                     )
                     
                     
                     
            ),
            
            tabPanel("Geopolitical Risk", 
                     h2("Indice de risque géopolitique (GPR)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 12 --------
                     tags$div(style = "margin: 30px; text-align: center;",
                              div(style = "display: inline-block; width: 80%; border: 1px solid #ccc; border-radius: 8px;
                                    padding: 20px; background-color: white; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                                  plotOutput("plot_gpr_alt", height = "400px")
                              )
                     ),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",h6("Statistiques descriptives",
                                                      style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_gpr_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_geo2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_gpr2"),
                             verbatimTextOutput("test_seasdum_gpr2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_gpr2"),
                             verbatimTextOutput("test_kpss_gpr2")
                      )
                     ),
                     
            ),
            
            tabPanel("Climat", 
                     h2("Anomalies de température (en°C)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 13 --------
                     radioButtons("graph_choice_climat2", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_climat_all2", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;", gt_output("table_stat_climat_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_climat2", height = "300px", width = "500px")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_climat"),
                             verbatimTextOutput("test_seasdum_climat")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_climat"),
                             verbatimTextOutput("test_kpss_climat")
                      ),
                      
                     ),
            )
           )
          ),
          
          conditionalPanel(
           condition = "input.var_filter == 'exo'",
           tabsetPanel(
            tabPanel("Prix Pétrole", 
                     h2("Prix du pétrole brut (USD/baril)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 7 --------
                     radioButtons("graph_choice_petrole", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_petrole", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_petrole_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_petrole_alt", height = "300px", width = "500px")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_petrole"),
                             verbatimTextOutput("test_seasdum_petrole")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_petrole"),
                             verbatimTextOutput("test_kpss_petrole")
                      ),
                     )
                     
                     
            ),
            
            tabPanel("Tx change EUR/USD", 
                     h2("Taux de change EUR/USD", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 8 --------
                     radioButtons("graph_choice_change", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_change", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_tauxchange_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_fx1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_change"),
                             verbatimTextOutput("test_seasdum_change")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_change"),
                             verbatimTextOutput("test_kpss_change")
                      ),
                     )
                     
                     
            ),
            
            tabPanel("Importation combustibles", 
                     h2("Importations de combustibles (en millions d’euros)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 9 --------
                     radioButtons("graph_choice_import1", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_import1", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_combustibles_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_combustibles1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_combustibles"),
                             verbatimTextOutput("test_seasdum_combustibles")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_combustibles"),
                             verbatimTextOutput("test_kpss_combustibles")
                      ),
                     )
                     
                     
            ),
            
            tabPanel("Prix Mondial Gaz", 
                     h2("Prix mondial du gaz naturel (UE)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 10 --------
                     radioButtons("graph_choice_gaz_1", "Transformation :",
                                  choices = c("Série brute" = "brute", "Corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_gaz_1", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_gaz_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_gaz1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_gaz"),
                             verbatimTextOutput("test_seasdum_gaz")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_gaz"),
                             verbatimTextOutput("test_kpss_gaz")
                      ),
                     )
                     
            ),
            
            tabPanel("FAO Food Index", 
                     h2("Indice FAO des prix des produits alimentaires", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 11 --------
                     radioButtons("graph_choice_fao_1", "Transformation :",
                                  choices = c("Série brute" = "brute", "Corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_fao_1", height = "400px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                      width: fit-content; margin: auto;",
                                 h6("Statistiques descriptives",
                                    style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_fao_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_fao1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_fao"),
                             verbatimTextOutput("test_seasdum_fao")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_fao"),
                             verbatimTextOutput("test_kpss_fao")
                      ),
                     )
                     
            ),
            
            tabPanel("Risque géopolitique", 
                     h2("Indice de risque géopolitique (GPR)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 12 --------
                     tags$div(style = "margin: 30px; text-align: center;",
                              div(style = "display: inline-block; width: 80%; border: 1px solid #ccc; border-radius: 8px;
                                    padding: 20px; background-color: white; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                                  plotOutput("plot_gpr", height = "400px")
                              )
                     ),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",h6("Statistiques descriptives",
                                                      style = "font-weight: bold; margin-bottom: 20px; text-align: center;"),
                                 gt_output("table_stat_gpr_2")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_geo1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_gpr"),
                             verbatimTextOutput("test_seasdum_gpr")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_gpr"),
                             verbatimTextOutput("test_kpss_gpr")
                      ),
                     )
                     
            ),
            
            tabPanel("Climat", 
                     h2("Anomalies de température (en°C)", style = "text-align: center; margin-top: 60px;"),
                     #### -Variable 13 --------
                     radioButtons("graph_choice_climat", "Transformation de la série :",
                                  choices = c("Série brute" = "brute", "Série corrigée & différenciée" = "corr"),
                                  inline = TRUE),
                     plotOutput("plot_choisi_climat", height = "350px"),
                     h2("Analyse Descriptive de la Série Brute", style = "text-align: center; margin-top: 60px;"),
                     
                     fluidRow(
                      column(width = 12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px; 
                box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white; 
                width: fit-content; margin: auto;",gt_output("table_stat_climat_1")
                             )
                      )
                     ),
                     tags$div(style = "height: 60px;"),
                     
                     fluidRow(
                      column(12, align = "center",
                             div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;
              width: fit-content; margin: auto;",
                                 plotOutput("boxplot_climat1", height = "300px", width = "500px")
                             )
                      )
                     ),
                     
                     fluidRow(
                      column(width = 6,
                             verbatimTextOutput("test_combined_climat2"),
                             verbatimTextOutput("test_seasdum_climat2")
                      ),
                      column(width = 6,
                             verbatimTextOutput("test_adf_climat2"),
                             verbatimTextOutput("test_kpss_climat2")
                      )
                     )
                     
            )
           )
          )
          
 ),
 
 # Rowlin ----
 tabPanel("Rowlin", h3("Page Rowlin à développer"),
          sidebarLayout(
           sidebarPanel(
            selectInput("choix_composante", "Choisir une composante :", choices = c(
             "01.1.1 - Pain et céréales" = "01_1_1",
             "01.1.2 - Viande" = "01_1_2",
             "01.1.3 - Poissons & fruits de mer" = "01_1_3",
             "01.1.4 - Lait, fromage, œufs" = "01_1_4",
             "01.1.5 - Huiles et graisses" = "01_1_5",
             "01.1.6 - Fruits" = "01_1_6",
             "01.1.7 - Légumes" = "01_1_7",
             "01.1.8 - Sucre, chocolat, confiserie" = "01_1_8",
             "01.1.9 - Produits n.c.a." = "01_1_9",
             "01.2.1 - Café, thé, cacao" = "01_2_1",
             "01.2.2 - Eaux, jus, boissons" = "01_2_2"
            )),
            tags$hr(),
            strong("Poids de la composante :"),
            textOutput("poids_composante")
           ),
           mainPanel(
            plotOutput("graph_prevision")
           )
          ),
          br(),
          h4("Prévision agrégée vs Réel"),
          plotOutput("graph_agg")
 )
)

# Serveur -----
server <- function(input, output, session) {
 output$graph_p1 <- renderPlot({
  pA
 })
 
 output$plot_choisi_1 <- renderPlot({
  if (input$graph_choice_1 == "brute") {
   autoplot(ts_Pain_cereales) +
    ggtitle("IPC - Coicop : 01.1.1 - Pain et céréales (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_1_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.1 - Pain et céréales (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_2 <- renderPlot({
  if (input$graph_choice_2 == "brute") {
   autoplot(ts_Viande) +
    ggtitle("IPC - Coicop : 01.1.2 - Viande (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_2_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.2 - Viande (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_3 <- renderPlot({
  if (input$graph_choice_3 == "brute") {
   autoplot(ts_Poissons_fruitsdemer) +
    ggtitle("IPC - Coicop : 01.1.3 - Poissons et fruits de mer (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_3_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.3 - Poissons et fruits de mer (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_11 <- renderPlot({
  if (input$graph_choice_11 == "brute") {
   autoplot(ts_Eauxminerales_boissonsrafraîchissantes) +
    ggtitle("IPC - Coicop : 01.2.2 - Eaux minérales, boissons et jus (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_2_2_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.2.2 - Eaux minérales, boissons et jus (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })  # ← cette parenthèse manquait
 
 
 output$plot_choisi_4 <- renderPlot({
  if (input$graph_choice_4 == "brute") {
   autoplot(ts_Lait_fromage_oeufs) +
    ggtitle("IPC - Coicop : 01.1.4 - Lait, fromage et œufs (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_4_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.4 - Lait, fromage et œufs (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_5 <- renderPlot({
  if (input$graph_choice_5 == "brute") {
   autoplot(ts_Huiles_graisses) +
    ggtitle("IPC - Coicop : 01.1.5 - Huiles et graisses (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_5_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.5 - Huiles et graisses (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_6 <- renderPlot({
  if (input$graph_choice_6 == "brute") {
   autoplot(ts_Fruits) +
    ggtitle("IPC - Coicop : 01.1.6 - Fruits (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_6_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.6 - Fruits (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_7 <- renderPlot({
  if (input$graph_choice_7 == "brute") {
   autoplot(ts_Legumes) +
    ggtitle("IPC - Coicop : 01.1.7 - Légumes (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_7_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.7 - Légumes (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_8 <- renderPlot({
  if (input$graph_choice_8 == "brute") {
   autoplot(ts_Sucre_confiture_confiserie) +
    ggtitle("IPC - Coicop : 01.1.8 - Sucre, confiture, confiserie (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_8_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.8 - Sucre, confiture, confiserie (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_9 <- renderPlot({
  if (input$graph_choice_9 == "brute") {
   autoplot(ts_n_c_a) +
    ggtitle("IPC - Coicop : 01.1.9 - Produits alimentaires n.c.a. (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_1_9_corr_diff) +
    ggtitle("IPC - Coicop : 01.1.9 - Produits alimentaires n.c.a. (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_10 <- renderPlot({
  if (input$graph_choice_10 == "brute") {
   autoplot(ts_Cafe_the_cacao) +
    ggtitle("IPC - Coicop : 01.2.1 - Café, thé et cacao (série brute)") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_2_1_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.2.1 - Café, thé et cacao (corrigée & différenciée)") +
    xlab("Date") + ylab("IPC différencié") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_11 <- renderPlot({
  if (input$graph_choice_11 == "brute") {
   autoplot(ts_Eauxminerales_boissonsrafraîchissantes) +
    ggtitle("IPC - Coicop : 01.2.2 - Eaux minérales, boissons et jus (série brute)") +
    xlab("Date") + ylab("IPC - 01.2.2") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(tsIPC01_2_2_CVS_RJDmetra_corr_diff) +
    ggtitle("IPC - Coicop : 01.2.2 - Eaux minérales, boissons et jus (série corrigée & différenciée)") +
    xlab("Date") + ylab("IPC 01.2.2") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 # Graphique des prev modele IPC 01,01
 observeEvent(input$select_all_1_1_1, {
  if (input$select_all_1_1_1) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_1", 
                            selected = unique(df_all_01_1_1$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_1", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_1 <- renderPlot({
  req(input$modeles_selectionnes_1_1_1)
  
  df_filtre <- df_all_01_1_1 %>%
   filter(model %in% input$modeles_selectionnes_1_1_1)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.1 : Pain et céréales - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 
 
 output$plot_choisi_climat <- renderPlot({
  if (input$graph_choice_climat == "brute") {
   autoplot(ts_Climat) +
    ggtitle("Anomalies de température - (2010–2023)") +
    xlab("Date") + ylab("Anomalie (°C)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Climat_CVS_diff) +
    ggtitle("Anomalies de température - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Anomalie (°C)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 
 output$plot_choisi_climat_all2 <- renderPlot({
  if (input$graph_choice_climat2 == "brute") {
   autoplot(ts_Climat) +
    ggtitle("Anomalies de température - (2010–2023)") +
    xlab("Date") + ylab("Anomalie (°C)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Climat_CVS_diff) +
    ggtitle("Anomalies de température - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Anomalie (°C)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$table_stat_inflation_us <- render_gt({
  tibble::tibble(
   Statistique = c(
    "N obs", "Min", "Max", "1er Quartile", "3e Quartile",
    "Moyenne", "Médiane", "Somme", "SE Moyenne",
    "IC Inférieure Moyenne", "IC Supérieure Moyenne",
    "Variance", "Écart-type", "Asymétrie", "Kurtosis"
   ),
   Valeur = c(
    167, 79.867060, 109.218217, 86.700945, 98.827843,
    92.847565, 92.572589, 15505.543296, 0.556876,
    91.748092, 93.947037, 51.788497, 7.196423,
    0.159263, -1.137180
   )
  ) %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.961, p-value = 0.0001278",
    locations = cells_body(columns = Valeur, rows = 1)
   )
 })
 
 # ts_Climat
 output$boxplot_climat1 <- renderPlot({
  boxplot(ts_Climat,
          main = "Boîte à moustaches - Climat",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_climat2 <- renderPlot({
  boxplot(ts_Climat,
          main = "Boîte à moustaches - Climat",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_Geopolitical_Risk
 output$boxplot_geo1 <- renderPlot({
  boxplot(ts_Geopolitical_Risk,
          main = "Boîte à moustaches - Risque géopolitique",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_geo2 <- renderPlot({
  boxplot(ts_Geopolitical_Risk,
          main = "Boîte à moustaches - Risque géopolitique",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_Inflation_US
 output$boxplot_inflation1 <- renderPlot({
  boxplot(ts_Inflation_US,
          main = "Boîte à moustaches - Inflation US",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_inflation2 <- renderPlot({
  boxplot(ts_Inflation_US,
          main = "Boîte à moustaches - Inflation US",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_FAO_Food_Index
 output$boxplot_fao1 <- renderPlot({
  boxplot(ts_FAO_Food_Index,
          main = "Boîte à moustaches - Indice FAO",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_fao2 <- renderPlot({
  boxplot(ts_FAO_Food_Index,
          main = "Boîte à moustaches - Indice FAO",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_PrixMondialGaz
 output$boxplot_gaz1 <- renderPlot({
  boxplot(ts_PrixMondialGaz,
          main = "Boîte à moustaches - Prix mondial du gaz",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_gaz2 <- renderPlot({
  boxplot(ts_PrixMondialGaz,
          main = "Boîte à moustaches - Prix mondial du gaz",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_Imporation_combustibles
 output$boxplot_combustibles1 <- renderPlot({
  boxplot(ts_Imporation_combustibles,
          main = "Boîte à moustaches - Importation combustibles",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_combustibles2 <- renderPlot({
  boxplot(ts_Imporation_combustibles,
          main = "Boîte à moustaches - Importation combustibles ",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_Tx_change_EUR_USD
 output$boxplot_fx1 <- renderPlot({
  boxplot(ts_Tx_change_EUR_USD,
          main = "Boîte à moustaches - Taux de change EUR/USD",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_fx2 <- renderPlot({
  boxplot(ts_Tx_change_EUR_USD,
          main = "Boîte à moustaches - Taux de change EUR/USD",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 # ts_Prix_Petrole
 output$boxplot_petrole1 <- renderPlot({
  boxplot(ts_Prix_Petrole,
          main = "Boîte à moustaches - Prix du pétrole",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_petrole2 <- renderPlot({
  boxplot(ts_Prix_Petrole,
          main = "Boîte à moustaches - Prix du pétrole",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 output$boxplot_petrole_alt <- renderPlot({
  boxplot(ts_Prix_Petrole,
          main = "Boîte à moustaches - Prix du pétrole",
          ylab = "Valeurs",
          col = "lightgreen")
 })
 
 
 # Graphique 1.2 ----
 observeEvent(input$select_all_1_1_2, {
  if (input$select_all_1_1_2) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_2", 
                            selected = unique(df_all_01_1_2$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_2", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_2 <- renderPlot({
  req(input$modeles_selectionnes_1_1_2)
  
  df_filtre <- df_all_01_1_2 %>%
   filter(model %in% input$modeles_selectionnes_1_1_2)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.2 : Viande - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 }) 
 
 # Graphique des prev modele IPC 03 Poissons et fruits de mer 
 observeEvent(input$select_all_1_1_3, {
  if (input$select_all_1_1_3) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_3", 
                            selected = unique(df_all_01_1_3$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_3", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_3 <- renderPlot({
  req(input$modeles_selectionnes_1_1_3)
  
  df_filtre <- df_all_01_1_3 %>%
   filter(model %in% input$modeles_selectionnes_1_1_3)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.3 : Poissons et fruits de mer  - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 04 Lait, fromage et œufs 
 observeEvent(input$select_all_1_1_4, {
  if (input$select_all_1_1_4) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_4", 
                            selected = unique(df_all_01_1_4$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_4", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_4 <- renderPlot({
  req(input$modeles_selectionnes_1_1_4)
  
  df_filtre <- df_all_01_1_4 %>%
   filter(model %in% input$modeles_selectionnes_1_1_4)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.4 : Lait, fromage et œufs   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 05 Huiles et graisses 
 observeEvent(input$select_all_1_1_5, {
  if (input$select_all_1_1_5) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_5", 
                            selected = unique(df_all_01_1_5$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_5", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_5 <- renderPlot({
  req(input$modeles_selectionnes_1_1_5)
  
  df_filtre <- df_all_01_1_5 %>%
   filter(model %in% input$modeles_selectionnes_1_1_5)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.5 : Huiles et graisses   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 05 Fruits
 observeEvent(input$select_all_1_1_6, {
  if (input$select_all_1_1_6) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_6", 
                            selected = unique(df_all_01_1_6$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_6", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_6 <- renderPlot({
  req(input$modeles_selectionnes_1_1_6)
  
  df_filtre <- df_all_01_1_6 %>%
   filter(model %in% input$modeles_selectionnes_1_1_6)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.5 : Fruits   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 07 Légumes
 observeEvent(input$select_all_1_1_7, {
  if (input$select_all_1_1_7) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_7", 
                            selected = unique(df_all_01_1_7$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_7", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_7 <- renderPlot({
  req(input$modeles_selectionnes_1_1_7)
  
  df_filtre <- df_all_01_1_7 %>%
   filter(model %in% input$modeles_selectionnes_1_1_7)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.7 : Légumes   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 08 Sucre, confiture et confiserie
 observeEvent(input$select_all_1_1_8, {
  if (input$select_all_1_1_8) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_8", 
                            selected = unique(df_all_01_1_8$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_8", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_8 <- renderPlot({
  req(input$modeles_selectionnes_1_1_8)
  
  df_filtre <- df_all_01_1_8 %>%
   filter(model %in% input$modeles_selectionnes_1_1_8)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.8 : Sucre, confiture et confiserie   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 09 Produits alimentaires n.c.a.
 observeEvent(input$select_all_1_1_9, {
  if (input$select_all_1_1_9) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_9", 
                            selected = unique(df_all_01_1_9$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_1_9", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_1_9 <- renderPlot({
  req(input$modeles_selectionnes_1_1_9)
  
  df_filtre <- df_all_01_1_9 %>%
   filter(model %in% input$modeles_selectionnes_1_1_9)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.1.9 : Produits alimentaires n.c.a.   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 21 Café, thé et cacao
 observeEvent(input$select_all_1_2_1, {
  if (input$select_all_1_2_1) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_2_1", 
                            selected = unique(df_all_01_2_1$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_2_1", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_2_1 <- renderPlot({
  req(input$modeles_selectionnes_1_2_1)
  
  df_filtre <- df_all_01_2_1 %>%
   filter(model %in% input$modeles_selectionnes_1_2_1)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.2.1 : Café, thé et cacao   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique des prev modele IPC 22 Eaux minérales, boissons et jus
 observeEvent(input$select_all_1_2_2, {
  if (input$select_all_1_2_2) {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_2_2", 
                            selected = unique(df_all_01_2_2$model))
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes_1_2_2", 
                            selected = NULL)
  }
 })
 output$graph_previsions_1_2_2 <- renderPlot({
  req(input$modeles_selectionnes_1_2_2)
  
  df_filtre <- df_all_01_2_2 %>%
   filter(model %in% input$modeles_selectionnes_1_2_2)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions - 01.2.2 : Eaux minérales, boissons et jus   - Sur 12 mois (2023)",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ADAM_ETS" = "red",
    "ADAM_ETS+SARIMA" = "orange",
    "HoltWinters" = "yellow",
    "Naïve" = "black",
    "SSARIMA" = "purple",
    "X13" = "darkblue",
    "ARMA" = "forestgreen",
    "AR(1)" = "pink",
    "AR(P)" = "darkcyan",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "HoltWinters" = "solid",
    "Naïve" = "solid",
    "SSARIMA" = "solid",
    "X13" = "dashed",
    "ARMA" = "dotted",
    "AR(1)" = "solid",
    "AR(P)" = "dotted",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 
 # 2. Graphique A : Affichage de la série brute ou corrigée
 output$graph_selected <- renderPlot({
  if (input$graph_choice == "brute") {
   autoplot(tsIPC01) +
    ggtitle("Indice des prix à la consommation (IPC 01) – Série brute") +
    xlab("Date") + ylab("IPC") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts1IPC01_CVS_RJDmetra_corr_diff) +
    ggtitle("Indice des prix à la consommation (IPC 01) – Série corrigée & différenciée") +
    xlab("Date") + ylab("IPC (diff)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 # Mise à jour automatique du groupe selon la case "Tous les modèles"
 observeEvent(input$select_all, {
  if (input$select_all) {
   updateCheckboxGroupInput(session, "modeles_selectionnes", selected = modeles_disponibles)
  } else {
   updateCheckboxGroupInput(session, "modeles_selectionnes", selected = NULL)
  }
 })
 
 # Graphique B
 output$graph_previsions <- renderPlot({
  req(input$modeles_selectionnes)  # Au moins un modèle
  
  df_filtre <- df_all_01 %>%
   filter(model %in% input$modeles_selectionnes)
  
  ggplot(df_filtre, aes(x = date, y = value, color = model, linetype = model)) +
   geom_line(size = 1.2) +
   labs(
    title = "Prévisions sur 12 mois (2023) selon les modèles sélectionnés",
    x = "Mois", y = "Valeur prévue", color = "Modèle", linetype = "Modèle"
   ) +
   scale_color_manual(values = c(
    "ARMA" = "forestgreen",
    "AR(1)" = "brown",
    "AR(P)" = "darkcyan",
    "X13" = "darkblue",
    "STL" = "yellow",
    "HoltWinters" = "orange",
    "ADAM_ETS" = "grey20",
    "ADAM_ETS+SARIMA" = "red",
    "SSARIMA" = "purple",
    "Désagrégée" = "darkred",
    "Random forest" = "deeppink",
    "ARX" = "deeppink",
    "Naïve" = "black",
    "Observée" = "blue"
   )) +
   scale_linetype_manual(values = c(
    "ARMA" = "solid",
    "AR(1)" = "dotted",
    "AR(P)" = "dotted",
    "X13" = "dashed",
    "STL" = "solid",
    "HoltWinters" = "solid",
    "ADAM_ETS" = "solid",
    "ADAM_ETS+SARIMA" = "solid",
    "SSARIMA" = "solid",
    "Désagrégée" = "solid",
    "Random forest" = "dotdash",
    "ARX" = "solid",
    "Naïve" = "solid",
    "Observée" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
   )
 })
 
 # Graphique CSPE filtré
 output$graph_cspe <- renderPlot({
  
  # Fonction mise à jour : transforme la date en format mois
  extract_forecast_df <- function(fcast_obj, name) {
   df <- data.frame(
    date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
    value = as.numeric(fcast_obj$mean),
    model = name
   )
   df %>% slice_head(n = 12)
  }
  
  ts_forex13_01 <- ts(as.numeric(forex13_01), start = c(2023, 1), frequency = 12)
  
  # 2. Construire un objet forecast manuellement (sans recalculer)
  forecast_x13_01 <- structure(list(
   mean = ts_forex13_01,  # ici, un vrai ts avec start/end
   lower = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
   upper = matrix(NA, nrow = 12, ncol = 2, dimnames = list(NULL, c("80%", "95%"))),
   level = c(80, 95),
   x = NULL,
   method = "X13",
   fitted = NULL,
   residuals = NULL
  ), class = "forecast")
  
  prev_rf$mean <- ts(prev_rf$mean, start = c(2023, 1), frequency = 12)
  
  dfs_01 <- list(
   extract_forecast_df(prev_arima, "ARMA"),
   extract_forecast_df(prev_ar1, "AR(1)"),
   extract_forecast_df(prev_arP, "AR(P)"),
   extract_forecast_df(forecast_x13_01, "X13"),
   extract_forecast_df(prevstl, "STL"),
   extract_forecast_df(forecast_hw, "HoltWinters"),
   extract_forecast_df(prev_ADAM_ETS, "ADAM_ETS"),
   extract_forecast_df(prev_AES, "ADAM_ETS+SARIMA"),
   extract_forecast_df(prev_SSARIMA, "SSARIMA"),
   extract_forecast_df(pred_ipc_disagg, "Désagrégée"),
   extract_forecast_df(prev_rf, "Random forest"),
   extract_forecast_df(pred_naive, "Naïve"),
   extract_forecast_df(forecast_armax, "ARX"))
  
  #Je vais comparer aveclesvaleur reel grace a ts_test01
  observed_df_01 <- data.frame(
   date = as.Date(as.yearmon(time(ts_test01))),
   value = as.numeric(ts_test01),
   model = "Observée")
  
  # Combine toutes les prévisions et l'observée
  df_all_01 <- bind_rows(dfs_01, observed_df_01)
  df_all_01$date <- as.Date(df_all_01$date, format = "%Y-%m-%d")
  library(ggplot2)
  
  req(input$cspe_modeles)
  
  df_cspe_filtre <- cspe_df_long %>%
   filter(Model %in% input$cspe_modeles)
  
  ggplot(df_cspe_filtre, aes(x = Date, y = CSPE, color = Model, linetype = Model)) +
   geom_line(size = 0.8) +
   labs(title = "Cumulative Squared Prediction Errors (CSPE)",
        x = "Date", y = "CSPE", color = "Modèle", linetype = "Modèle") +
   scale_color_manual(values = c(
    "ARMA" = "forestgreen", "AR(1)" = "brown", "AR(P)" = "darkcyan",
    "X13" = "darkblue", "STL" = "gold", "HoltWinters" = "orange",
    "ADAM_ETS" = "grey20", "ADAM_ETS+SARIMA" = "red", "SSARIMA" = "purple",
    "Désagrégée" = "darkred", "Random forest" = "deeppink", "Naïve" = "black",
    "ARX" = "pink"
   )) +
   scale_linetype_manual(values = c(
    "ARMA" = "solid", "AR(1)" = "dotted", "AR(P)" = "dotted",
    "X13" = "dashed", "STL" = "solid", "HoltWinters" = "solid",
    "ADAM_ETS" = "solid", "ADAM_ETS+SARIMA" = "solid",
    "SSARIMA" = "solid", "Désagrégée" = "solid",
    "Random forest" = "dotdash", "Naïve" = "solid", "ARX" = "solid"
   )) +
   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
   theme_minimal(base_size = 13) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "bottom",
         legend.title = element_text(face = "bold"),
         plot.title = element_text(face = "bold"))
 })
 
 # ---- Données pour le tableau de performance
 performance_df_01 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(1.294679, 0.3050297, 0.3514895, 0.9470476, 2.62493, 2.794945, 3.602854, 0.4545674, 1.178839),
  r2oos = c(-0.09826608, 0.7412457, 0.7018343, 0.196627, -1.226708, -1.37093, -2.056273, 0.6143941, 0)
 )
 
 output$table_performance_01 <- render_gt({
  performance_df_01 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_1)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "AR(1)")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano
 dm_df_01 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.04715, 0.001316, 0.001437, 0.001382,
   0.001199, 0.002186, 0.002433, 0.001663
  )
 )
 
 output$table_dm_01 <- render_gt({
  dm_df_01 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_1)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_1_2
 performance_df_01_2 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.3409916, 0.1269334, 0.6417995, 1.03406, 0.5019379, 1.236952, 1.312447, 0.4667771, 0.9160816),
  r2oos = c(0.6277716, 0.4904634, 0.299408, -0.1287858, 0.4520816, -0.3502643, -0.4326749, 0.8614388, 0)
 )
 
 output$table_performance_01_2 <- render_gt({
  performance_df_01_2 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_2)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "SSARIMA")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_2
 dm_df_01_2 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.0005414, 0.0008316, 0.002632, 0.0007005,
   0.0006275, 0.0004994, 0.0009855, 0.000558
  )
 )
 
 output$table_dm_01_2 <- render_gt({
  dm_df_01_2 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_2)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 
 
 # Table performance
 output$table_performance <- render_gt({
  performance_df %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles de prévision**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "Désagrégée")
   )
 })
 
 # Table Diebold-Mariano
 output$table_dm <- render_gt({
  dm_test_df %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 dm_df_01_4 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.003861, 0.0186, 0.005526, 0.001801,
   0.09231, 0.01473, 0.5775, 0.002903
  )
 )
 
 output$table_dm_01_4 <- render_gt({
  dm_df_01_4 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_4)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 performance_df_01_4 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.7354304, 0.2486782, 0.3810932, 0.903208, 0.4576065, 1.366877, 1.054826, 0.5945084, 1.145275),
  r2oos = c(0.3578567, 0.7828659, 0.6672473, 0.2113612, 0.6004395, -0.1934932, 0.07897506, 0.4809032, 0)
 )
 
 output$table_performance_01_4 <- render_gt({
  performance_df_01_4 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_4)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "SSARIMA")
   )
 })
 
 # ---- Données pour le tableau de performance : 01_1_3
 # ---- Données pour le tableau de performance : 01_1_3
 performance_df_01_3 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.5431028, 0.5054832, 0.6269941, 1.179119, 3.035049, 1.322039, 1.071687, 0.5326092, 3.005585),
  r2oos = c(0.8193021, 0.8318187, 0.7913903, 0.6076907, -0.009803145, 0.5601392, 0.6434348, 0.8227935, 0)
 )
 
 output$table_performance_01_3 <- render_gt({
  performance_df_01_3 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_3)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "SSARIMA")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_3
 dm_df_01_3 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.0001297, 0.005448, 0.0007294, 0.0003888,
   0.0004786, 0.003145, 0.002919, 0.007371
  )
 )
 
 output$table_dm_01_3 <- render_gt({
  dm_df_01_3 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_3)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 fluidRow(
  column(width = 6,
         div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
             h4("Comparaison des performances des modèles (01_1_3)", style = "font-weight: bold; margin-bottom: 20px;"),
             gt_output("table_performance_01_3")
         )
  ),
  column(width = 6,
         div(style = "border: 1px solid #ccc; border-radius: 8px; padding: 20px;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background-color: white;",
             h4("Test de Diebold-Mariano (01_1_3)", style = "font-weight: bold; margin-bottom: 20px;"),
             gt_output("table_dm_01_3")
         )
  )
 )
 # ---- Données pour le tableau de performance : 01_1_5
 performance_df_01_5 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(1.154037, 0.6106392, 0.7864602, 0.6497492, 0.6343057, 0.4154186, 1.609307, 0.7951129, 1.286506),
  r2oos = c(0.102968, 0.5253506, 0.388685, 0.4949504, 0.5069546, 0.6770954, -0.2509131, 0.3819593, 0)
 )
 
 output$table_performance_01_5 <- render_gt({
  performance_df_01_5 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_5)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "ADAM_ETS")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_5
 dm_df_01_5 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.8652, 0.229, 0.4293, 0.00139,
   0.006044, 0.002834, 0.0003231, 0.444
  )
 )
 
 output$table_dm_01_5 <- render_gt({
  dm_df_01_5 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_5)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 # ---- Données pour le tableau de performance : 01_1_6
 performance_df_01_6 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(2.964004, 2.965762, 3.070568, 2.651247, 9.106402, 2.763622, 3.01094, 2.649948, 7.782676),
  r2oos = c(0.6191536, 0.6189278, 0.6054612, 0.6593399, -0.1700862, 0.6449008, 0.6131228, 0.6595068, 0)
 )
 
 output$table_performance_01_6 <- render_gt({
  performance_df_01_6 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_6)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "SSARIMA")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_6
 dm_df_01_6 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.003847, 0.007103, 0.006613, 0.008937,
   0.04151, 0.01002, 0.005313, 0.002409
  )
 )
 
 output$table_dm_01_6 <- render_gt({
  dm_df_01_6 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_6)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_1_7
 performance_df_01_7 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(10.32893, 9.901548, 10.10968, 10.32893, 21.19238, 13.0329, 16.71038, 11.60533, 18.11368),
  r2oos = c(0.4297718, 0.4533662, 0.4418757, 0.4297718, -0.1699655, 0.2804939, 0.0774716, 0.3593055, 0)
 )
 
 output$table_performance_01_7 <- render_gt({
  performance_df_01_7 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_7)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "AR(1)")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_7
 dm_df_01_7 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.1518, 0.1236, 0.1399, 0.1518,
   0.2685, 0.3357, 0.7964, 0.1538
  )
 )
 
 output$table_dm_01_7 <- render_gt({
  dm_df_01_7 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_7)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_1_8
 performance_df_01_8 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.4464516, 0.1705691, 0.4760385, 0.4935622, 3.639235, 0.7674818, 1.637723, 0.4635147, 1.101923),
  r2oos = c(0.5948432, 0.8452078, 0.567993, 0.5520901, -2.302621, 0.303507, -0.4862409, 0.5793584, 0)
 )
 
 output$table_performance_01_8 <- render_gt({
  performance_df_01_8 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_8)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "AR(1)")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_8
 dm_df_01_8 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.007441, 0.02187, 0.01061, 0.007751,
   0.1151, 0.01354, 0.006851, 0.007417
  )
 )
 
 output$table_dm_01_8 <- render_gt({
  dm_df_01_8 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_8)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_1_9
 performance_df_01_9 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.6611652, 0.5464891, 0.3141904, 0.55895, 0.5295206, 0.7585702, 0.7148, 0.6159393, 0.4218022),
  r2oos = c(-0.567477, -0.2956053, 0.2551237, -0.3251474, -0.2553766, -0.7984029, -0.6946333, -0.4602563, 0)
 )
 
 output$table_performance_01_9 <- render_gt({
  performance_df_01_9 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_1_9)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "AR(P)")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_1_9
 dm_df_01_9 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.1501, 0.555, 0.009702, 0.2379,
   0.2241, 0.06976, 0.1323, 0.1997
  )
 )
 
 output$table_dm_01_9 <- render_gt({
  dm_df_01_9 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_1_9)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_2_1
 performance_df_01_2_1 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.7817841, 0.1095468, 0.1891133, 0.6267331, 1.399729, 0.562633, 1.022511, 0.5962472, 0.5166349),
  r2oos = c(-0.5132234, 0.787961, 0.6339517, -0.2131062, -1.70932, -0.08903392, -0.9791744, -0.1540977, 0)
 )
 
 output$table_performance_01_2_1 <- render_gt({
  performance_df_01_2_1 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_2_1)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "AR(1)")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_2_1
 dm_df_01_2_1 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.001449, 0.04243, 0.01038, 0.001915,
   0.03521, 0.3513, 0.002014, 0.02574
  )
 )
 
 output$table_dm_01_2_1 <- render_gt({
  dm_df_01_2_1 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_2_1)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # ---- Données pour le tableau de performance : 01_2_2 (X13 en vedette)
 performance_df_01_2_2 <- data.frame(
  Modèle = c("ARMA", "AR(1)", "AR(P)", "X13", "HoltWinters", "ADAM_ETS", "ADAM_ETS+SARIMA", "SSARIMA", "Naïve"),
  MSE = c(0.1578406, 0.3868626, 0.1596869, 0.1578491, 0.3991533, 0.3578334, 0.1517146, 0.2123195, 0.2675865),
  r2oos = c(0.4101324, -0.4457477, 0.4032328, 0.4101006, -0.4916795, -0.3372625, 0.4330259, 0.206539, 0)
 )
 
 output$table_performance_01_2_2 <- render_gt({
  performance_df_01_2_2 %>%
   gt() %>%
   tab_header(title = md("**Comparaison des performances des modèles (01_2_2)**")) %>%
   fmt_number(columns = c("MSE", "r2oos"), decimals = 3) %>%
   cols_label(
    Modèle = "Modèle",
    MSE = "MSE",
    r2oos = md("R²<sub>OOS</sub>")
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Modèle == "X13")
   )
 })
 
 # ---- Données pour le tableau Diebold-Mariano : 01_2_2 (h = 1 uniquement)
 dm_df_01_2_2 <- data.frame(
  Modèles = c(
   "Naïve vs ARMA", "Naïve vs AR(1)", "Naïve vs AR(P)", "Naïve vs X13",
   "Naïve vs HoltWinters", "Naïve vs ADAM_ETS", "Naïve vs ADAM_ETS+SARIMA",
   "Naïve vs SSARIMA"
  ),
  p_value = c(
   0.005361, 0.358, 0.001808, 0.005364,
   0.2267, 0.3199, 0.01283, 0.4282
  )
 )
 
 output$table_dm_01_2_2 <- render_gt({
  dm_df_01_2_2 %>%
   gt() %>%
   tab_header(title = md("**Test de Diebold-Mariano (01_2_2)**")) %>%
   fmt_number(columns = "p_value", decimals = 4) %>%
   cols_label(
    Modèles = "Modèles",
    p_value = "p-value"
   ) %>%
   tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.1)
   )
 })
 
 # Graphique GPR
 output$plot_gpr <- renderPlot({
  autoplot(ts_Geopolitical_Risk) +
   ggtitle("Indice de risque géopolitique (GPR) - (2010–2023)") +
   xlab("Date") + ylab("Indice GPR") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 output$plot_gpr_alt <- renderPlot({
  autoplot(ts_Geopolitical_Risk) +
   ggtitle("Indice de risque géopolitique (GPR) - (2010–2023)") +
   xlab("Date") + ylab("Indice GPR") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 output$plot_choisi_fao_1 <- renderPlot({
  if (input$graph_choice_fao_1 == "brute") {
   autoplot(ts_FAO_Food_Index) +
    ggtitle("Indice FAO des prix des produits alimentaires - (2010–2023)") +
    xlab("Date") + ylab("FAO Food Index") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_FAO_Food_Index_diff) +
    ggtitle("Indice FAO des prix des produits alimentaires - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("FAO Food Index (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_fao_2 <- renderPlot({
  if (input$graph_choice_fao_2 == "brute") {
   autoplot(ts_FAO_Food_Index) +
    ggtitle("Indice FAO des prix des produits alimentaires - (2010–2023)") +
    xlab("Date") + ylab("FAO Food Index") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_FAO_Food_Index_diff) +
    ggtitle("Indice FAO des prix des produits alimentaires - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("FAO Food Index (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_gaz_1 <- renderPlot({
  if (input$graph_choice_gaz_1 == "brute") {
   autoplot(ts_PrixMondialGaz) +
    ggtitle("Prix mondial du gaz naturel (UE) - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_PrixMondialGaz_diff) +
    ggtitle("Prix mondial du gaz naturel (UE) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_gaz_2 <- renderPlot({
  if (input$graph_choice_gaz_2 == "brute") {
   autoplot(ts_PrixMondialGaz) +
    ggtitle("Prix mondial du gaz naturel (UE) - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_PrixMondialGaz_diff) +
    ggtitle("Prix mondial du gaz naturel (UE) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_gaz <- renderPlot({
  if (input$graph_choice_gaz == "brute") {
   autoplot(ts_PrixMondialGaz) +
    ggtitle("Prix mondial du gaz naturel (UE) - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (USD)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_PrixMondialGaz_diff) +
    ggtitle("Prix mondial du gaz naturel (UE) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_gaz2 <- renderPlot({
  if (input$graph_choice_gaz2 == "brute") {
   autoplot(ts_PrixMondialGaz) +
    ggtitle("Prix mondial du gaz naturel (UE) - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (USD)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_PrixMondialGaz_diff) +
    ggtitle("Prix mondial du gaz naturel (UE) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix mondial du gaz (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_change <- renderPlot({
  if (input$graph_choice_change == "brute") {
   autoplot(ts_Tx_change_EUR_USD) +
    ggtitle("Taux de change EUR/USD - (2010–2023)") +
    xlab("Date") + ylab("Taux de change EUR/USD") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Tx_change_EUR_USD_diff) +
    ggtitle("Taux de change EUR/USD - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Taux de change EUR/USD (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 output$plot_choisi_change2 <- renderPlot({
  if (input$graph_choice_change2 == "brute") {
   autoplot(ts_Tx_change_EUR_USD) +
    ggtitle("Taux de change EUR/USD - (2010–2023)") +
    xlab("Date") + ylab("Taux de change EUR/USD") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Tx_change_EUR_USD_diff) +
    ggtitle("Taux de change EUR/USD - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Taux de change EUR/USD (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 
 output$plot_choisi_petrole <- renderPlot({
  if (input$graph_choice_petrole == "brute") {
   autoplot(ts_Prix_Petrole) +
    ggtitle("Prix du pétrole brut (USD/baril) - (2010–2023)") +
    xlab("Date") + ylab("Prix du pétrole (USD)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Prix_Petrole_diff) +
    ggtitle("Prix du pétrole brut (USD/baril) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix du pétrole (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 output$plot_choisi_petrole2 <- renderPlot({
  if (input$graph_choice_petrole2 == "brute") {
   autoplot(ts_Prix_Petrole) +
    ggtitle("Prix du pétrole brut (USD/baril) - (2010–2023)") +
    xlab("Date") + ylab("Prix du pétrole (USD)") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Prix_Petrole_diff) +
    ggtitle("Prix du pétrole brut (USD/baril) - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Prix du pétrole (différencié)") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 
 # Premier exemplaire
 output$plot_choisi_import1 <- renderPlot({
  if (input$graph_choice_import1 == "brute") {
   autoplot(ts_Imporation_combustibles) +
    ggtitle("Importations de combustibles - (2010–2023)") +
    xlab("Date") + ylab("Importation_combustibles") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Imporation_combustibles_CVS_diff) +
    ggtitle("Importations de combustibles - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Importation_combustibles") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 # Deuxième exemplaire
 output$plot_choisi_import2 <- renderPlot({
  if (input$graph_choice_import2 == "brute") {
   autoplot(ts_Imporation_combustibles) +
    ggtitle("Importations de combustibles - (2010–2023)") +
    xlab("Date") + ylab("Importation_combustibles") +
    theme_minimal() +
    geom_line(color = "blue")
  } else {
   autoplot(ts_Imporation_combustibles_CVS_diff) +
    ggtitle("Importations de combustibles - série CVS et différenciée - (2010–2023)") +
    xlab("Date") + ylab("Importation_combustibles") +
    theme_minimal() +
    geom_line(color = "blue")
  }
 })
 
 # Graphique 1 : Taux directeur
 output$plot_taux_directeur <- renderPlot({
  autoplot(ts_Taux_directeur) +
   ggtitle("Taux directeur de la BCE - (2010–2023)") +
   xlab("Date") + ylab("Taux directeur") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 # Graphique 2 : IPP
 output$plot_ipp <- renderPlot({
  autoplot(ts_IPP) +
   ggtitle("Indices de prix de production et d’importation - (2010–2023)") +
   xlab("Date") + ylab("IPP") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 # Graphique 3 : Coût horaire
 output$plot_cout_horaire <- renderPlot({
  autoplot(ts_cout_horaire) +
   ggtitle("Coût horaire du travail - (2010–2023)") +
   xlab("Date") + ylab("Coût horaire") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 # Graphique 4 : Consommation des ménages
 output$plot_conso_menages <- renderPlot({
  autoplot(ts_consommation_ménages_alim) +
   ggtitle("Consommation des ménages en biens alimentaires - (2010–2023)") +
   xlab("Date") + ylab("Consommation") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 # Graphique 5 : Confiance des ménages
 output$plot_confiance_menages <- renderPlot({
  autoplot(ts_confiance_menages) +
   ggtitle("Indice de confiance des ménages - (2010–2023)") +
   xlab("Date") + ylab("Confiance des ménages") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 # Graphique 6 : ts_Inflation_US
 output$plot_inflation_us <- renderPlot({
  autoplot(ts_Inflation_US) +
   ggtitle("Indice des prix à la consommation aux États-Unis (IPC) - (2010-2023)") +
   xlab("Date") + 
   ylab("IPPAP") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 
 # Graphique 6 : IPPAP
 output$plot_ippap <- renderPlot({
  autoplot(ts_IPPAP) +
   ggtitle("Indice des prix agricoles à la production - (2010–2023)") +
   xlab("Date") + ylab("IPPAP") +
   theme_minimal() +
   geom_line(color = "blue")
 })
 
 output$table_stat_desc <- render_gt({
  stat_desc_df %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.616, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything()) # option : déplacer ici si tu veux éviter le titre
   )
 })
 
 output$table_stat_ipp <- render_gt({
  stat_desc_ipp %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.639, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 
 output$table_stat_cout <- render_gt({
  stat_desc_cout_horaire %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.934, p-value = 5.843e-07",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_conso_alim <- render_gt({
  stat_desc_conso_alim %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 2) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.945, p-value = 4.145e-06",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_confiance <- render_gt({
  tibble::tibble(
   Statistique = c("N obs", "Min", "Max", "1er Quartile", "3e Quartile",
                   "Moyenne", "Médiane", "Somme", "SE Moyenne",
                   "IC Inférieure Moyenne", "IC Supérieure Moyenne",
                   "Variance", "Écart-type", "Asymétrie", "Kurtosis"),
   Valeur = c(167, 79.867060, 109.218217, 86.700945, 98.827843,
              92.847565, 92.572589, 15505.543296, 0.556876,
              91.748092, 93.947037, 51.788497, 7.196423, 0.159263, -1.137180)
  ) %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.961, p-value = 0.0001278",
    locations = cells_body(columns = Valeur, rows = 1)
   )
 })
 
 output$table_stat_ippap <- render_gt({
  stat_desc_ippap %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.830, p-value = 1.126e-12",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 
 # Premier usage
 output$table_stat_climat_1 <- render_gt({
  stat_desc_climat %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.963, p-value = 0.0001811",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Deuxième usage
 output$table_stat_climat_2 <- render_gt({
  stat_desc_climat %>%
   gt() %>%
   fmt_number(columns = where(is.numeric), decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.963, p-value = 0.0001811",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_gpr_1 <- render_gt({
  stat_desc_gpr %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.711, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_gpr_2 <- render_gt({
  stat_desc_gpr %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.711, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_inflation_1 <- render_gt({
  stat_desc_inflation %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.895, p-value = 1.445e-09",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_inflation_2 <- render_gt({
  stat_desc_inflation %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.895, p-value = 1.445e-09",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_fao_1 <- render_gt({
  stat_desc_fao %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.920, p-value = 5.272e-08",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 output$table_stat_fao_2 <- render_gt({
  stat_desc_fao %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.920, p-value = 5.272e-08",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Tableau 1
 output$table_stat_gaz_1 <- render_gt({
  stat_desc_gaz %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.645, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Tableau 2
 output$table_stat_gaz_2 <- render_gt({
  stat_desc_gaz %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.645, p-value < 2.2e-16",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Premier tableau
 output$table_stat_combustibles_1 <- render_gt({
  stat_desc_combustibles %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.888, p-value = 6.098e-10",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Deuxième tableau
 output$table_stat_combustibles_2 <- render_gt({
  stat_desc_combustibles %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.888, p-value = 6.098e-10",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 
 # Premier tableau
 output$table_stat_tauxchange_1 <- render_gt({
  stat_desc_tauxchange %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.954, p-value = 2.758e-05",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Deuxième tableau
 output$table_stat_tauxchange_2 <- render_gt({
  stat_desc_tauxchange %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.954, p-value = 2.758e-05",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 
 # Premier tableau
 output$table_stat_petrole_1 <- render_gt({
  stat_desc_petrole %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.957, p-value = 5.249e-05",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Deuxième tableau
 output$table_stat_petrole_2 <- render_gt({
  stat_desc_petrole %>%
   gt() %>%
   fmt_number(columns = "Valeur", decimals = 3) %>%
   cols_label(
    Statistique = "Statistique",
    Valeur = "Valeur"
   ) %>%
   tab_footnote(
    footnote = "Test de normalité Shapiro-Wilk : W = 0.957, p-value = 5.249e-05",
    locations = cells_column_labels(columns = everything())
   )
 })
 
 # Boxplots
 output$boxplot_ipp <- renderPlot({
  boxplot(ts_IPP, main = "Boîte à moustaches : IPP", ylab = "Valeurs", col = "lightgreen")
 })
 
 output$boxplot_taux_directeur <- renderPlot({
  boxplot(ts_Taux_directeur, main = "Boîte à moustaches : Taux Directeur", ylab = "Valeurs", col = "lightgreen")
 })
 
 output$boxplot_cout_horaire <- renderPlot({
  boxplot(ts_cout_horaire, main = "Boîte à moustaches : Coût Horaire", ylab = "Valeurs", col = "lightgreen")
 })
 
 output$boxplot_conso_alim <- renderPlot({
  boxplot(ts_consommation_ménages_alim, main = "Boîte à moustaches : Consommation alimentaire", ylab = "Valeurs", col = "lightgreen")
 })
 
 output$boxplot_confiance <- renderPlot({
  boxplot(ts_confiance_menages, main = "Boîte à moustaches : Confiance des ménages", ylab = "Valeurs", col = "lightgreen")
 })
 
 output$boxplot_ippap <- renderPlot({
  boxplot(ts_IPPAP, main = "Boîte à moustaches : IPPAP", ylab = "Valeurs", col = "lightgreen")
 })
 
 # Boxplot Inflation US
 output$boxplot_inflation_us <- renderPlot({
  boxplot(
   ts_Inflation_US,
   main = "Boîte à moustaches de la série",
   ylab = "Valeurs",
   col = "lightgreen"
  )
 })
 
 
 # Résultats des tests ----
 output$test_combined <- renderPrint({
  combined_test(ts1IPC01_CVS_RJDmetra_corr_diff)
 })
 
 output$test_seasdum <- renderPrint({
  seasdum(ts1IPC01_CVS_RJDmetra_corr_diff)
 })
 
 output$test_adf <- renderPrint({
  adf.test(ts1IPC01_CVS_RJDmetra_corr_diff)
 })
 
 output$test_kpss <- renderPrint({
  kpss.test(ts1IPC01_CVS_RJDmetra_corr_diff)
 })
 
 # ---- Tests stationnarité/saisonnalité ----
 
 # Série 01_1_1
 output$test_combined_1_1 <- renderPrint({ combined_test(tsIPC01_1_1_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_1 <- renderPrint({ seasdum(tsIPC01_1_1_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_1     <- renderPrint({ adf.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_1    <- renderPrint({ kpss.test(tsIPC01_1_1_CVS_RJDmetra_corr_diff) })
 
 # Série 01_1_2
 output$test_combined_1_2 <- renderPrint({ combined_test(tsIPC01_1_2_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_2 <- renderPrint({ seasdum(tsIPC01_1_2_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_2     <- renderPrint({ adf.test(tsIPC01_1_2_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_2    <- renderPrint({ kpss.test(tsIPC01_1_2_CVS_RJDmetra_corr_diff) })
 
 # Série 01_1_3
 output$test_combined_1_3 <- renderPrint({ combined_test(tsIPC01_1_3_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_3 <- renderPrint({ seasdum(tsIPC01_1_3_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_3     <- renderPrint({ adf.test(tsIPC01_1_3_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_3    <- renderPrint({ kpss.test(tsIPC01_1_3_CVS_RJDmetra_corr_diff) })
 
 # Série 01_1_4
 output$test_combined_1_4 <- renderPrint({ combined_test(tsIPC01_1_4_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_4 <- renderPrint({ seasdum(tsIPC01_1_4_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_4     <- renderPrint({ adf.test(tsIPC01_1_4_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_4    <- renderPrint({ kpss.test(tsIPC01_1_4_CVS_RJDmetra_corr_diff) })
 
 # Série 01_1_5
 output$test_combined_1_5 <- renderPrint({ combined_test(tsIPC01_1_5_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_5 <- renderPrint({ seasdum(tsIPC01_1_5_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_5     <- renderPrint({ adf.test(tsIPC01_1_5_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_5    <- renderPrint({ kpss.test(tsIPC01_1_5_CVS_RJDmetra_corr_diff) })
 
 # Série 01_1_6
 output$test_combined_1_6 <- renderPrint({ combined_test(tsIPC01_1_6_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_6 <- renderPrint({ seasdum(tsIPC01_1_6_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_6     <- renderPrint({ adf.test(tsIPC01_1_6_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_6    <- renderPrint({ kpss.test(tsIPC01_1_6_CVS_RJDmetra_corr_diff) })
 
 # ---- Composante 01_1_7 
 output$test_combined_1_7 <- renderPrint({ combined_test(tsIPC01_1_7_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_7  <- renderPrint({ seasdum(tsIPC01_1_7_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_7      <- renderPrint({ adf.test(tsIPC01_1_7_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_7     <- renderPrint({ kpss.test(tsIPC01_1_7_CVS_RJDmetra_corr_diff) })
 
 # ---- Composante 01_1_8
 output$test_combined_1_8 <- renderPrint({ combined_test(tsIPC01_1_8_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_1_8  <- renderPrint({ seasdum(tsIPC01_1_8_CVS_RJDmetra_corr_diff) })
 output$test_adf_1_8      <- renderPrint({ adf.test(tsIPC01_1_8_CVS_RJDmetra_corr_diff) })
 output$test_kpss_1_8     <- renderPrint({ kpss.test(tsIPC01_1_8_CVS_RJDmetra_corr_diff) })
 
 # ---- Composante 01_1_9
 output$test_combined_1_9 <- renderPrint({ combined_test(tsIPC01_1_9_corr_diff) })
 output$test_seasdum_1_9  <- renderPrint({ seasdum(tsIPC01_1_9_corr_diff) })
 output$test_adf_1_9      <- renderPrint({ adf.test(tsIPC01_1_9_corr_diff) })
 output$test_kpss_1_9     <- renderPrint({ kpss.test(tsIPC01_1_9_corr_diff) })
 
 # ---- Composante 01_2_1
 output$test_combined_2_1 <- renderPrint({ combined_test(tsIPC01_2_1_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_2_1  <- renderPrint({ seasdum(tsIPC01_2_1_CVS_RJDmetra_corr_diff) })
 output$test_adf_2_1      <- renderPrint({ adf.test(tsIPC01_2_1_CVS_RJDmetra_corr_diff) })
 output$test_kpss_2_1     <- renderPrint({ kpss.test(tsIPC01_2_1_CVS_RJDmetra_corr_diff) })
 
 # ---- Composante 01_2_2
 output$test_combined_2_2 <- renderPrint({ combined_test(tsIPC01_2_2_CVS_RJDmetra_corr_diff) })
 output$test_seasdum_2_2  <- renderPrint({ seasdum(tsIPC01_2_2_CVS_RJDmetra_corr_diff) })
 output$test_adf_2_2      <- renderPrint({ adf.test(tsIPC01_2_2_CVS_RJDmetra_corr_diff) })
 output$test_kpss_2_2     <- renderPrint({ kpss.test(tsIPC01_2_2_CVS_RJDmetra_corr_diff) })
 
 output$formule <- renderUI({
  HTML('
      <p>La prévision agrégée est obtenue par :</p>
      <p>$$\\hat{\\pi}_t = \\sum_{i=1}^{11} w_i \\hat{y}_{i,t}$$</p>
      <p>où \\(w_i\\) est le poids de la composante \\(i\\), et \\(\\hat{y}_{i,t}\\) sa valeur prédite.</p>
    ')
 })
 
 # Outliers -----
 output$plot_outliers <- renderPlot({
  plot(outliers01)
 })
 
 output$outliers_resume <- renderPrint({
  show(outliers01)
 })
 
 # ---- Outliers pour 01_1_1
 output$plot_outliers_1_1 <- renderPlot({
  plot(outliers01_1_1)
 })
 
 output$outliers_resume_1_1 <- renderPrint({
  show(outliers01_1_1)
 })
 
 # ---- Outliers pour 01_1_2
 output$plot_outliers_1_2 <- renderPlot({
  plot(outliers01_1_2)
 })
 
 output$outliers_resume_1_2 <- renderPrint({
  show(outliers01_1_2)
 })
 # Composante 01_1_3
 output$plot_outliers_01_1_3 <- renderPlot({ plot(outliers01_1_3) })
 output$outliers_resume_01_1_3 <- renderPrint({ show(outliers01_1_3) })
 
 # Composante 01_1_4
 output$plot_outliers_01_1_4 <- renderPlot({ plot(outliers01_1_4) })
 output$outliers_resume_01_1_4 <- renderPrint({ show(outliers01_1_4) })
 
 # Composante 01_1_5
 output$plot_outliers_01_1_5 <- renderPlot({ plot(outliers01_1_5) })
 output$outliers_resume_01_1_5 <- renderPrint({ show(outliers01_1_5) })
 
 # Composante 01_1_6
 output$plot_outliers_01_1_6 <- renderPlot({ plot(outliers01_1_6) })
 output$outliers_resume_01_1_6 <- renderPrint({ show(outliers01_1_6) })
 
 # Composante 01_1_7
 output$plot_outliers_01_1_7 <- renderPlot({ plot(outliers01_1_7) })
 output$outliers_resume_01_1_7 <- renderPrint({ show(outliers01_1_7) })
 
 # Composante 01_1_8
 output$plot_outliers_01_1_8 <- renderPlot({ plot(outliers01_1_8) })
 output$outliers_resume_01_1_8 <- renderPrint({ show(outliers01_1_8) })
 
 # Composante 01_1_9
 output$plot_outliers_01_1_9 <- renderPlot({ plot(outliers01_1_9) })
 output$outliers_resume_01_1_9 <- renderPrint({ show(outliers01_1_9) })
 
 # Composante 01_2_1
 output$plot_outliers_01_2_1 <- renderPlot({ plot(outliers01_2_1) })
 output$outliers_resume_01_2_1 <- renderPrint({ show(outliers01_2_1) })
 
 # Composante 01_2_2
 output$plot_outliers_01_2_2 <- renderPlot({ plot(outliers01_2_2) })
 output$outliers_resume_01_2_2 <- renderPrint({ show(outliers01_2_2) })
 
 output$table_gt <- render_gt({
  create_gt_table(data)
 })
 
 # Corresponding server outputs
 output$test_combined_climat <- renderPrint({
  combined_test(ts_Climat_CVS_diff)
 })
 
 output$test_seasdum_climat <- renderPrint({
  seasdum(ts_Climat_CVS_diff)
 })
 
 output$test_adf_climat <- renderPrint({
  adf.test(ts_Climat_CVS_diff)
 })
 
 output$test_kpss_climat <- renderPrint({
  kpss.test(ts_Climat_CVS_diff)
 })
 
 output$test_combined_climat2 <- renderPrint({
  combined_test(ts_Climat_CVS_diff)
 })
 
 output$test_seasdum_climat2 <- renderPrint({
  seasdum(ts_Climat_CVS_diff)
 })
 
 output$test_adf_climat2 <- renderPrint({
  adf.test(ts_Climat_CVS_diff)
 })
 
 output$test_kpss_climat2 <- renderPrint({
  kpss.test(ts_Climat_CVS_diff)
 })
 
 # GPR - Bloc 1
 output$test_combined_gpr <- renderPrint({
  combined_test(ts_Geopolitical_Risk)
 })
 
 output$test_seasdum_gpr <- renderPrint({
  seasdum(ts_Geopolitical_Risk)
 })
 
 output$test_adf_gpr <- renderPrint({
  adf.test(ts_Geopolitical_Risk)
 })
 
 output$test_kpss_gpr <- renderPrint({
  kpss.test(ts_Geopolitical_Risk)
 })
 
 # GPR - Bloc 2 (duplicata)
 output$test_combined_gpr2 <- renderPrint({
  combined_test(ts_Geopolitical_Risk)
 })
 
 output$test_seasdum_gpr2 <- renderPrint({
  seasdum(ts_Geopolitical_Risk)
 })
 
 output$test_adf_gpr2 <- renderPrint({
  adf.test(ts_Geopolitical_Risk)
 })
 
 output$test_kpss_gpr2 <- renderPrint({
  kpss.test(ts_Geopolitical_Risk)
 })
 # FAO - Bloc 1
 output$test_combined_fao <- renderPrint({
  combined_test(ts_FAO_Food_Index_diff)
 })
 
 output$test_seasdum_fao <- renderPrint({
  seasdum(ts_FAO_Food_Index_diff)
 })
 
 output$test_adf_fao <- renderPrint({
  adf.test(ts_FAO_Food_Index_diff)
 })
 
 output$test_kpss_fao <- renderPrint({
  kpss.test(ts_FAO_Food_Index_diff)
 })
 
 # FAO - Bloc 2
 output$test_combined_fao2 <- renderPrint({
  combined_test(ts_FAO_Food_Index_diff)
 })
 
 output$test_seasdum_fao2 <- renderPrint({
  seasdum(ts_FAO_Food_Index_diff)
 })
 
 output$test_adf_fao2 <- renderPrint({
  adf.test(ts_FAO_Food_Index_diff)
 })
 
 output$test_kpss_fao2 <- renderPrint({
  kpss.test(ts_FAO_Food_Index_diff)
 })
 
 # Gaz - Bloc 1
 output$test_combined_gaz <- renderPrint({
  combined_test(ts_PrixMondialGaz_diff)
 })
 
 output$test_seasdum_gaz <- renderPrint({
  seasdum(ts_PrixMondialGaz_diff)
 })
 
 output$test_adf_gaz <- renderPrint({
  adf.test(ts_PrixMondialGaz_diff)
 })
 
 output$test_kpss_gaz <- renderPrint({
  kpss.test(ts_PrixMondialGaz_diff)
 })
 
 # Gaz - Bloc 2
 output$test_combined_gaz2 <- renderPrint({
  combined_test(ts_PrixMondialGaz_diff)
 })
 
 output$test_seasdum_gaz2 <- renderPrint({
  seasdum(ts_PrixMondialGaz_diff)
 })
 
 output$test_adf_gaz2 <- renderPrint({
  adf.test(ts_PrixMondialGaz_diff)
 })
 
 output$test_kpss_gaz2 <- renderPrint({
  kpss.test(ts_PrixMondialGaz_diff)
 })
 
 # Importation combustibles - Bloc 1
 output$test_combined_combustibles <- renderPrint({
  combined_test(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_seasdum_combustibles <- renderPrint({
  seasdum(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_adf_combustibles <- renderPrint({
  adf.test(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_kpss_combustibles <- renderPrint({
  kpss.test(ts_Imporation_combustibles_CVS_diff)
 })
 
 # Importation combustibles - Bloc 2
 output$test_combined_combustibles2 <- renderPrint({
  combined_test(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_seasdum_combustibles2 <- renderPrint({
  seasdum(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_adf_combustibles2 <- renderPrint({
  adf.test(ts_Imporation_combustibles_CVS_diff)
 })
 
 output$test_kpss_combustibles2 <- renderPrint({
  kpss.test(ts_Imporation_combustibles_CVS_diff)
 })
 
 # Taux de change EUR/USD - Bloc 1
 output$test_combined_change <- renderPrint({
  combined_test(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_seasdum_change <- renderPrint({
  seasdum(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_adf_change <- renderPrint({
  adf.test(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_kpss_change <- renderPrint({
  kpss.test(ts_Tx_change_EUR_USD_diff)
 })
 
 # Taux de change EUR/USD - Bloc 2
 output$test_combined_change2 <- renderPrint({
  combined_test(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_seasdum_change2 <- renderPrint({
  seasdum(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_adf_change2 <- renderPrint({
  adf.test(ts_Tx_change_EUR_USD_diff)
 })
 
 output$test_kpss_change2 <- renderPrint({
  kpss.test(ts_Tx_change_EUR_USD_diff)
 })
 
 # Prix du pétrole - Bloc 1
 output$test_combined_petrole <- renderPrint({
  combined_test(ts_Prix_Petrole_diff)
 })
 
 output$test_seasdum_petrole <- renderPrint({
  seasdum(ts_Prix_Petrole_diff)
 })
 
 output$test_adf_petrole <- renderPrint({
  adf.test(ts_Prix_Petrole_diff)
 })
 
 output$test_kpss_petrole <- renderPrint({
  kpss.test(ts_Prix_Petrole_diff)
 })
 
 # Prix du pétrole - Bloc 2
 output$test_combined_petrole2 <- renderPrint({
  combined_test(ts_Prix_Petrole_diff)
 })
 
 output$test_seasdum_petrole2 <- renderPrint({
  seasdum(ts_Prix_Petrole_diff)
 })
 
 output$test_adf_petrole2 <- renderPrint({
  adf.test(ts_Prix_Petrole_diff)
 })
 
 output$test_kpss_petrole2 <- renderPrint({
  kpss.test(ts_Prix_Petrole_diff)
 })
 
 
 # Mapping identifiant -> index poids
 composantes <- c("01_1_1", "01_1_2", "01_1_3", "01_1_4", "01_1_5", "01_1_6", 
                  "01_1_7", "01_1_8", "01_1_9", "01_2_1", "01_2_2")
 poids_map <- setNames(weights, composantes)
 
 output$poids_composante <- renderText({
  poids <- poids_map[[input$choix_composante]]
  paste0(round(poids * 100, 2), " %")
 })
 
 output$graph_prevision <- renderPlot({
  df <- switch(input$choix_composante,
               "01_1_1" = compare_01_1_1,
               "01_1_2" = compare_01_1_2,
               "01_1_3" = compare_01_1_3,
               "01_1_4" = compare_01_1_4,
               "01_1_5" = compare_01_1_5,
               "01_1_6" = compare_01_1_6,
               "01_1_7" = compare_01_1_7,
               "01_1_8" = compare_01_1_8,
               "01_1_9" = compare_01_1_9,
               "01_2_1" = compare_01_2_1,
               "01_2_2" = compare_01_2_2_rolling)
  
  col_pred <- grep("Prévision", names(df), value = TRUE)
  titre <- titre_modele[[input$choix_composante]][1]
  modele <- titre_modele[[input$choix_composante]][2]
  
  plot(1:12, df$Valeur_réelle, type = "l", col = "black", lwd = 2,
       xlab = "Mois", ylab = "Valeur différenciée",
       main = paste("Prévision rolling", modele, "vs Réel"),
       xaxt = "n",
       ylim = range(c(df$Valeur_réelle, df[[col_pred]])) * c(0.95, 1.05),
       xlim = c(0.5, 12.5))
  
  axis(1, at = 1:12, labels = month.abb, las = 2)
  lines(1:12, df[[col_pred]], col = "blue", lwd = 2, lty = 2)
  
  mtext(paste(input$choix_composante, "-", titre), side = 3, line = 0.5, adj = 0, cex = 1.1, font = 2)
 })
 output$graph_agg <- renderPlot({
  plot(1:12, compare_agrégé$Valeur_réelle, type = "l", col = "black", lwd = 2,
       xlab = "Mois", ylab = "IPC (valeur corrigée)",
       main = "Prévision rolling agrégée vs Réel - IPC Produits alimentaires",
       xaxt = "n",
       ylim = range(c(compare_agrégé$Valeur_réelle, compare_agrégé$Prévision_agrégée)) * c(0.95, 1.05),
       xlim = c(0.5, 12.5))
  
  axis(1, at = 1:12, labels = month.abb, las = 2)
  lines(1:12, compare_agrégé$Prévision_agrégée, col = "blue", lwd = 2, lty = 2)
  legend("topright", legend = c("Réel", "Prévision agrégée"), col = c("black", "blue"),
         lty = c(1, 2), lwd = 2)
 })
 
 
}
# App ----
shinyApp(ui = ui, server = server)

