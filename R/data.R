#' HOMA_IR dataset
#'
#' Lipoprotein and physical activity data of 836 children between age 10 and 12 (1)
#'
#' Outcome - Homeostatic model assessment of insulin resistance (HOMA-IR)\cr
#' HOMA-IR was calculated as fastening serum insulin times fasting serum glucose divided by 22.5 (2)
#'
#' Explanatory variables - Physical activity spectrum\cr
#' Physical activity (PA) data was obtained using the ActiGraph GT3X+ accelerometer (3) worn at the waist over seven consecutive days,
#' except during water activities (swimming, showering) or while sleeping. We use a PA descriptor of 23 intervals covering the intensity spectrum of the vertical axis.
#' The intervals used for the descriptor were 0–99, 100–249, 250–499, 500–999, 1000–1499, 1500–1999, 2000–2499, 2500–2999,
#' 3000–3499, 3500–3999, 4000–4499, 4500–4999, 5000–5499, 5500-5999, 6000–6499, 6500-6999, 7000–7499, 7500-7999, 8000-8499,
#' 8500-8999, 9000-9499, 9500-9999 and greater than 10000 counts per minute (cpm). Time (min/day) spent in each of the PA intensities was calculated for the children.
#'
#' Explanatory variables – Adiposity\cr
#' We used three measures of adiposity: Body mass index (BMI), the ratio of waist circumference to height (WC_to_H), and skinfold thickness derived from measurement at
#' four places (biceps, triceps, subscapular, and suprailiac.
#'
#' Confounders - Age and Sex
#' Age is measured as a variable. The subjects are all 5th graders so the age range is narrow . The cohort includes both boys and girls so sex is
#' included as a binary variable; 0 for girls and 1 for boys, to be able to make one joint model incorporating both genders .
#'
#' Mediator - Lipoprotein features\cr
#' Serum lipoprotein profiles were characterized by 26 measures: Concentrations of total cholesterol (TC), triglyceride (TG), chylomicrons (CM), very-low-density lipoproteins (VLDL),
#' low-density lipoproteins (LDL), high-density lipoproteins (HDL), two subclasses of CM (CM-1 and CM-2), five subclasses of VLDL (VLDL-L1, VLDL-L2, VLDL-L3, VLDL-M,
#' VLDL-S), four subclasses of LDL (LDL-L, LDL-M, LDL-S, LDL-VS), six subclasses of HDL (HDL-VL1, HDL-VL2, HDL-L, HDL-M, HDL-S and HDL-VS), and the average particle size of VLDL, LDL
#' and HDL subclasses.
#'
#' (1) Rajalahti, Tarja; Aadland, Eivind; Resaland, Geir K.; Anderssen, Sigmund A.; Kvalheim, Olav M.\cr
#' Influence of adiposity and physical activity on the cardiometabolic association\cr
#' pattern of lipoprotein subclasses to aerobic fitness in prepubertal children.\cr
#' PLOS ONE, Vol. 16, No. 11, 2021, e0259901.
#'
#'
#' (2) John, Dinesh; Freedson Patty.\cr
#' Actigraph and Actical physical activity monitors:A peek under the hood.\cr
#' Medicine & Science in Sports & Exercise. Vol. 44, Issue 1S, 2012, S86–S89.
#'
#' @format A data frame with 836 rows and 55 variables:
#' \describe{
#' \item{HOMA_IR}{Homeostatic model assessment of insulin resistance, arbitrary unit}
#' \item{Age}{Age of the child, years, potential confounder}
#' \item{Sex}{Gender of the child, 1 - male | 0 - female, potential confounder}
#' \item{TC_P}{Total cholesterol}
#' \item{TG_P}{Triglycerides}
#' \item{CM_P}{Chylomicrons}
#' \item{VLDL_P}{Very-low-density lipoprotein}
#' \item{LDL_P}{Low-density lipoprotein}
#' \item{HDL_P}{High-density lipoprotein}
#' \item{CM_1_P}{Chylomicron 1}
#' \item{CM_2_P}{Chylomicron 2}
#' \item{VLDL_L1_P}{Very-low-density lipoprotein subclass L1}
#' \item{VLDL_L2_P}{Very-low-density lipoprotein subclass L2}
#' \item{VLDL_L3_P}{Very-low-density lipoprotein subclass L3}
#' \item{VLDL_M_P}{Very-low-density lipoprotein subclass M}
#' \item{VLDL_S_P}{Very-low-density lipoprotein subclass S}
#' \item{LDL_L_P}{Low-density lipoprotein subclass L}
#' \item{LDL_M_P}{Low-density lipoprotein subclass M}
#' \item{LDL_S_P}{Low-density lipoprotein subclass S}
#' \item{LDL_VS_P}{Low-density lipoprotein subclass VS}
#' \item{HDL_VL1_P}{High-density lipoprotein subclass VL1}
#' \item{HDL_VL2_P}{High-density lipoprotein subclass VL2}
#' \item{HDL_L_P}{High-density lipoprotein subclass L}
#' \item{HDL_M_P}{High-density lipoprotein subclass M}
#' \item{HDL_S_P}{High-density lipoprotein subclass S}
#' \item{HDL_VS_P}{High-density lipoprotein subclass VS}
#' \item{VLDL_Sizemol_P}{Average particle size of VLDL subclasses}
#' \item{LDL_Sizemol_P}{Average particle size of LDL subclasses}
#' \item{HDL_Sizemol_P}{Average particle size of HDL subclasses}
#' \item{BMI}{Body-mass-index, \eqn{\frac{kg}{m^{2}}}, potential confounder}
#' \item{WC_to_H}{Waist circumference to height ratio, arbitrary unit, potential confounder}
#' \item{Skinfold}{Measured skinfold, in cm, potential confounder, proxy for adiposity}
#' \item{PA_0_99}{Measured physical activity, arbitrary unit, range 0-99}
#' \item{PA_100_249}{Measured physical activity, arbitrary unit, range 100-249}
#' \item{PA_250_499}{Measured physical activity, arbitrary unit, range 250-499}
#' \item{PA_500_999}{Measured physical activity, arbitrary unit, range 500-999}
#' \item{PA_1000_1499}{Measured physical activity, arbitrary unit, range 1000-1499}
#' \item{PA_1500_1999}{Measured physical activity, arbitrary unit, range 1500-1999}
#' \item{PA_2000_2499}{Measured physical activity, arbitrary unit, range 2000-2499}
#' \item{PA_2500_2999}{Measured physical activity, arbitrary unit, range 2500-2999}
#' \item{PA_3000_3499}{Measured physical activity, arbitrary unit, range 3000-3499}
#' \item{PA_3500_3999}{Measured physical activity, arbitrary unit, range 3500-3999}
#' \item{PA_4000_4499}{Measured physical activity, arbitrary unit, range 4000-4499}
#' \item{PA_4500_4999}{Measured physical activity, arbitrary unit, range 4500-4999}
#' \item{PA_5000_5499}{Measured physical activity, arbitrary unit, range 5000-5499}
#' \item{PA_5500_5999}{Measured physical activity, arbitrary unit, range 5500-5999}
#' \item{PA_6000_6499}{Measured physical activity, arbitrary unit, range 6000-6499}
#' \item{PA_6500_6999}{Measured physical activity, arbitrary unit, range 6500-6999}
#' \item{PA_7000_7499}{Measured physical activity, arbitrary unit, range 7000-7499}
#' \item{PA_7500_7999}{Measured physical activity, arbitrary unit, range 7500-7999}
#' \item{PA_8000_8499}{Measured physical activity, arbitrary unit, range 8000-8499}
#' \item{PA_8500_8999}{Measured physical activity, arbitrary unit, range 8500-8999}
#' \item{PA_9000_9499}{Measured physical activity, arbitrary unit, range 9000-9499}
#' \item{PA_9500_9999}{Measured physical activity, arbitrary unit, range 9500-9999}
#' \item{PA_Min_10000}{Measured physical activity, arbitrary unit, range 10000-}
#' }
#' @source \url{https://doi.org/10.1371/journal.pone.0259901}
"HOMA_IR"
