library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(leaps)
library(caret)
library(glmnet)
library(reshape2)
library(corrplot)

# --------- Đọc dữ liệu ---------
data <- read.csv('C:/Users/Admin/năm ba/XLSLTK/TH/group/đồ án/apartments_for_rent_classified_10K.csv', encoding = 'latin1', sep = ';', na = c("", "NA", "N/A"))
head(data)
glimpse(data)
dim(data)

# ------------ EDA ------------

# HBac: tạo các bảng thống kê, vẽ một số biểu đồ và nhận xét. 

# Hiển thị thông tin cơ bản về dữ liệu
str(data)

# Nhận xét:
# Một số biến có nhiều giá trị thiếu (ví dụ: amenities, bathrooms, address).
# Một số biến cần chuyển đổi sang kiểu số (ví dụ: bathrooms, bedrooms, latitude, longitude).
# Một số biến có giá trị cố định hoặc ít biến đổi (ví dụ: currency, price_type, source).
# Biến address có nhiều giá trị "null", có thể cần xem xét để xử lý hoặc loại bỏ.
# Các biến như title, body có thể chứa thông tin chi tiết bổ sung nhưng cần xử lý dạng văn bản nếu sử dụng.

# Thống kê mô tả cho các biến số
summary(data)

# Kiểm tra dữ liệu thiếu
colSums(is.na(data) | data == "null" | data == "NA" | data == "") 

## Phân phối của một số biến
# Phân phối giá thuê
ggplot(data, aes(x = price)) + 
  geom_histogram(binwidth = 50, fill = "blue", color = "black") + 
  ggtitle("Phân phối giá thuê") + 
  xlab("Giá thuê (USD)") + 
  ylab("Tần suất")

# Dựa vào biểu đồ phân phối giá thuê, có thể nhận thấy một số điểm sau:
# Phân phối giá thuê có dạng phân phối không đối xứng (skewed distribution), với đường cong nghiêng về phía giá thuê thấp hơn.
# Giá thuê tập trung chủ yếu ở khoảng dưới 10,000 USD, với một số giá thuê cao hơn 30,000 USD nhưng số lượng rất ít.
# Có một "đỉnh" rõ rệt ở khoảng 0-100 USD, cho thấy có nhiều bất động sản có giá thuê rất thấp.
# Phân phối giá thuê có sự phân tán lớn, từ giá thuê rất thấp đến rất cao, cho thấy thị trường cho thuê bất động sản khá đa dạng.
# Nhìn chung, biểu đồ phản ánh một thị trường cho thuê bất động sản có sự phân hóa và phân tán giá thuê khá rõ rệt. Điều này có thể liên quan đến các yếu tố như vị trí, loại hình, chất lượng bất động sản và nhu cầu của khách hàng.

# Phân phối diện tích căn hộ
ggplot(data, aes(x = square_feet)) + 
  geom_histogram(binwidth = 10, fill = "green", color = "black") + 
  ggtitle("Phân phối diện tích căn hộ") + 
  xlab("Diện tích căn hộ (square feet)") + 
  ylab("Tần suất")

# Dựa vào biểu đồ phân phối diện tích căn hộ, có thể nhận thấy một số điểm sau:
# Phân phối diện tích căn hộ có dạng phân phối không đối xứng (skewed distribution), với đường cong nghiêng về phía diện tích thấp hơn.
# Diện tích căn hộ tập trung chủ yếu ở khoảng dưới 2,000 square feet, với một số căn hộ có diện tích lên đến khoảng 35,000 square feet nhưng số lượng rất ít.
# Có một "đỉnh" rõ rệt ở khoảng 0-500 square feet, cho thấy có nhiều căn hộ có diện tích rất nhỏ.
# Phân phối diện tích căn hộ có sự phân tán lớn, từ diện tích rất nhỏ đến rất lớn, cho thấy thị trường bất động sản có sự đa dạng về kích thước căn hộ.
# Nhìn chung, biểu đồ phản ánh một thị trường bất động sản có sự phân hóa và phân tán diện tích căn hộ khá rõ rệt. Điều này có thể liên quan đến các yếu tố như vị trí, mức độ xây dựng, nhu cầu của khách hàng và các chính sách quy hoạch của địa phương.

## Mối quan hệ giữa các biến
# Mối quan hệ giữa diện tích căn hộ và giá thuê
ggplot(data, aes(x = square_feet, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa diện tích căn hộ và giá thuê") + 
  xlab("Diện tích căn hộ (square feet)") + 
  ylab("Giá thuê (USD)")

# Dựa vào biểu đồ scatter plot mối quan hệ giữa diện tích căn hộ và giá thuê, có thể nhận thấy một số điểm sau:
# Có mối quan hệ tương đối tích cực giữa diện tích căn hộ và giá thuê, nghĩa là khi diện tích căn hộ tăng, giá thuê cũng có xu hướng tăng theo.
# Tuy nhiên, mối quan hệ này không hoàn toàn tuyến tính, mà khá phân tán. Có những căn hộ có diện tích lớn nhưng giá thuê không cao, và ngược lại.
# Phần lớn các điểm dữ liệu tập trung ở khu vực diện tích nhỏ hơn 20,000 square feet và giá thuê dưới 20,000 USD, cho thấy đây là phân khúc chiếm đa số trong thị trường.
# Vẫn có một số căn hộ có diện tích lên đến 35,000 square feet và giá thuê lên đến 40,000 USD, cho thấy sự đa dạng về quy mô và phân khúc giá trong thị trường bất động sản này.
# Nhìn chung, biểu đồ cho thấy diện tích căn hộ là một yếu tố ảnh hưởng đến giá thuê, nhưng không phải là yếu tố duy nhất. Các yếu tố khác như vị trí, chất lượng, tiện ích cũng đóng vai trò quan trọng trong xác định giá thuê bất động sản.

# Mối quan hệ giữa số phòng ngủ và giá thuê
ggplot(data, aes(x = bedrooms, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa số phòng ngủ và giá thuê") + 
  xlab("Số phòng ngủ") + 
  ylab("Giá thuê (USD)") + 
  theme_minimal()

# Có mối quan hệ tương đối tích cực giữa số phòng ngủ và giá thuê, nghĩa là khi số phòng ngủ tăng, giá thuê cũng có xu hướng tăng theo.
# Tuy nhiên, mối quan hệ này không hoàn toàn tuyến tính, mà khá phân tán. Có những căn hộ có số phòng ngủ nhiều nhưng giá thuê không cao, và ngược lại.
# Phần lớn các điểm dữ liệu tập trung ở khu vực số phòng ngủ từ 1 đến 5, với giá thuê chủ yếu dưới 20,000 USD, cho thấy đây là phân khúc chiếm đa số trong thị trường.
# Vẫn có một số ít căn hộ có số phòng ngủ lên đến 8 và 9, với giá thuê lên đến 30,000 USD, cho thấy sự đa dạng về quy mô và phân khúc giá trong thị trường bất động sản này.
# Một số điểm nằm ở "null" cho số phòng ngủ, có thể là những căn hộ studio hoặc không có thông tin về số phòng ngủ.

# Mối quan hệ giữa số phòng tắm và giá thuê
ggplot(data, aes(x = bathrooms, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa số phòng tắm và giá thuê") + 
  xlab("Số phòng tắm") + 
  ylab("Giá thuê (USD)") + 
  theme_minimal()

# Tương tự như số phòng ngủ, có mối quan hệ tích cực giữa số phòng tắm và giá thuê. Khi số phòng tắm tăng, giá thuê có xu hướng tăng theo.
# Tuy nhiên, mối quan hệ này không hoàn toàn tuyến tính và khá phân tán. Có nhiều điểm dữ liệu với số phòng tắm tương đương nhưng giá thuê lại khác biệt đáng kể.
# Phần lớn các điểm dữ liệu tập trung ở khu vực số phòng tắm từ 1 đến 4, với giá thuê chủ yếu dưới 20,000 USD. Đây có vẻ là phân khúc chính trong thị trường.
# Vẫn có một số ít căn hộ có số phòng tắm cao hơn, lên đến 6, 7 và 8, với giá thuê lên đến 30,000 USD. Điều này cho thấy sự đa dạng về quy mô và phân khúc giá trong thị trường.
# Một số điểm ở "null" cho số phòng tắm, có thể là những căn hộ không có thông tin về số phòng tắm.

# Tính số lượng các tiện ích khác nhau
amenities_counts <- table(data$amenities)

# Hiển thị 10 tiện ích phổ biến nhất
barplot(sort(amenities_counts, decreasing = TRUE)[1:10], 
        las = 2, 
        col = "purple", 
        main = "10 tiện ích phổ biến nhất", 
        ylab = "Số lượng")

# Tiện ích phổ biến nhất là "null", chiếm số lượng lớn nhất. Điều này có thể chỉ ra rằng một số nhà cho thuê có thông tin về tiện ích chưa đầy đủ.
# Tiện ích phổ biến tiếp theo là "Parking", "Conditioning Pool", "Washer" và "Refrigerator". Đây là những tiện ích cơ bản và phổ biến thường được cung cấp trong các thông tin cho thuê nhà.
# Các tiện ích như "Levigator", "Torage" và "ng.Storage" cũng có số lượng tương đối nhiều, cho thấy chúng là những tiện ích khá phổ biến.
# Một số tiện ích khác như "Lerigator" và "arkingPol" có số lượng ít hơn, tức là chúng ít phổ biến hơn trong các nhà cho thuê được liệt kê.

# ---------- Tiền xử lý dữ liệu -------------

# KThy: trình bày cụ thể các bước đang làm gì, giải thích tại sao lại xử lý như vậy...
# Cuối phần tiền xử lý có thể vẽ lại thêm 1 số biểu đồ như heatmap, phân tán j đó để nhận xét dữ liệu sau khi tiền xử lý
# Trả lời câu hỏi số 1&2 của thầy

## Loại bỏ các cột không cần thiết
# Do các cột chứa text không phải mục tiêu của chúng ta, 
# cột price_display mang giá trị lặp lại so với cột price
## không có sự phân biệt rõ ràng giữa các quan sát (như state)...
# Nên ra sẽ chọn ra những cột này để loại bỏ
columns_to_remove <- c("id", "category", "title", "body", "amenities", "currency", "fee", "has_photo", "price_display", 
                       "price_type", "pets_allowed", "address", "state", "latitude", "longitude", "source", "time")

# Loại bỏ các cột không cần thiết ở trên khỏi dữ liệu
data <- data[, !(names(data) %in% columns_to_remove)]


## Xử lý dữ liệu thiếu
# Thay thế các giá trị đặc biệt bằng NA trong cột bathrooms và bedrooms
data$bathrooms[data$bathrooms %in% c("null", "NA", "")] <- NA
data$bedrooms[data$bedrooms %in% c("null", "NA", "")] <- NA

# Chuyển đổi sang dạng numeric
data$bathrooms <- as.numeric(data$bathrooms)
data$bedrooms <- as.numeric(data$bedrooms)


## Vì số lượng các giá trị bị thiếu trong các cột này là rất ít so với số lượng mẫu mà ta có được
## Vậy nên thay vì thay thế các giá trị bị thiếu bằng 1 giá trị khác thì ta sẽ loại bỏ luôn
# Loại bỏ các hàng có giá trị thiếu trong cột bathrooms, bedrooms và cityname
data <- data[complete.cases(data$bathrooms, data$bedrooms), ]
# Loại bỏ các dòng có cityname là giá trị rỗng, "null", "NA", hoặc bất kỳ giá trị thiếu khác
data <- data[!grepl("^\\s*$|^null$|^NA$", data$cityname, ignore.case = TRUE), ]

# Kiểm tra lại dữ liệu thiếu sau khi loại bỏ
colSums(is.na(data) | data == "null" | data == "NA" | data == "")

print(dim(data))
summary(data)
head(data)

## Xử lý cột cityname
# Làm sạch dữ liệu trong cột cityname
data$cityname <- trimws(data$cityname)  # Loại bỏ khoảng trắng dư thừa ở đầu và cuối

# Đếm số lượng căn hộ cho thuê theo từng thành phố và sắp xếp giảm dần
location_stats <- data |>
  group_by(cityname) |>
  summarize(count = n()) |>
  arrange(desc(count))

location_stats

## Thực hiện gộp các thành phố có ít hơn 10 căn hộ vào nhóm "other" để:
## Giảm số lượng biến phân loại, làm cho mô hình đơn giản và dễ quản lý hơn
## Giảm thiểu biến động từ các thành phố có ít dữ liệu, giúp mô hình ổn định hơn
## Tránh overfitting khi có quá nhiều biến với ít dữ liệu.
# Đếm số lượng thành phố có số lượng căn hộ cho thuê ít hơn hoặc bằng 10
num_locations <- sum(location_stats$count <= 10)
num_locations

# Tạo bộ dữ liệu thống kê cho các thành phố có số lượng căn hộ cho thuê ít hơn hoặc bằng 10
location_stats_less_than_10 <- location_stats[location_stats$count <= 10, ]
location_stats_less_than_10

# Tạo một vector logic để xác định các thành phố nằm trong location_stats_less_than_10
is_less_than_10 <- data$cityname %in% location_stats_less_than_10$cityname

# Thay thế các thành phố thỏa điều kiện bằng nhãn "other"
data$cityname[is_less_than_10] <- "other"

# Đếm số lượng thành phố duy nhất
num_unique_cities <- length(unique(data$cityname))
num_unique_cities
print(dim(data))

## Lọc dữ liệu
# Lọc các hàng thỏa điều kiện diện tích chia cho số phòng ngủ nhỏ hơn 250
filtered_data <- data[data$square_feet / data$bedrooms < 250, ]
head(filtered_data)

# Lọc các hàng không thỏa điều kiện
df1 <- data[!(data$square_feet / data$bedrooms < 250), ]

# tính số hàng và cột của df1
dim(df1)

# Tạo bản sao của df1
df2 <- df1

# Tính toán giá trị trung bình của mỗi mét vuông (price per square foot) 
# từ hai cột dữ liệu là price (giá thuê) và square_feet (diện tích).
df2$price_per_sqft <- df2$price * 100000 / df2$square_feet

# Hiển thị 5 hàng đầu tiên của df2
head(df2)

## Loại bỏ outliers
# Tạo một hàm để loại bỏ ngoại lệ dựa trên mean và standard deviation của price_per_sqft theo từng thành phố
remove_pps_outliers <- function(df) {
  df_out <- data.frame() # Tạo DataFrame rỗng để lưu kết quả
  
  # Lặp qua từng nhóm thành phố
  city_stats <- df |>
    group_by(cityname) |>
    summarize(mean_pps = mean(price_per_sqft),
              std_pps = sd(price_per_sqft))
  
  # Lặp qua từng nhóm thành phố
  for (key in unique(df$cityname)) {
    subdf <- df[df$cityname == key, ] # Lấy các dòng dữ liệu của từng thành phố
    
    m <- city_stats$mean_pps[city_stats$cityname == key] # Lấy mean của thành phố hiện tại
    st <- city_stats$std_pps[city_stats$cityname == key] # Lấy standard deviation của thành phố hiện tại
    
    # Lọc các dòng dữ liệu ngoài phạm vi mean ± std
    reduced_df <- subdf[(subdf$price_per_sqft > (m - 3*st)) & (subdf$price_per_sqft <= (m + 3*st)), ]
    
    # Ghép reduced_df vào df_out
    df_out <- rbind(df_out, reduced_df)
  }
  
  return(df_out)
}

# Sử dụng hàm remove_pps_outliers để loại bỏ ngoại lệ trong df2
df3 <- remove_pps_outliers(df2)
dim(df3)

# Chỉ lấy các dòng trong df3 có số phòng ngủ lớn hơn 0
df3 <- df3[df3$bedrooms > 0, ]
dim(df3)

# Tính mean và standard deviation của square_feet và price
sqft_mean <- mean(df3$square_feet)
sqft_std <- sd(df3$square_feet)
price_mean <- mean(df3$price)
price_std <- sd(df3$price)

# Loại bỏ outliers (các dòng dữ liệu ngoài phạm vi mean ± std)
df3 <- df3[abs(df3$square_feet - sqft_mean) < 3 * sqft_std & 
             abs(df3$price - price_mean) < 3 * price_std, ]

# Loại bỏ các dòng có các giá trị không hợp lệ
df3 <- df3[df3$bathrooms >= 0 & 
             df3$bedrooms >= 0 & 
             df3$square_feet > 0 & 
             df3$price > 0, ]

# Log transform cho cột price
df3$price <- log(df3$price)

# Loại bỏ outliers cho cột price
df3 <- df3[df3$price < mean(df3$price) + 3 * sd(df3$price), ]

head(df3)

# Tính toán ma trận tương quan giữa các biến số
cor_matrix <- cor(df3[, sapply(df3, is.numeric)], method = "pearson")

# Chuyển ma trận tương quan thành dạng dài
melted_cor_matrix <- reshape2::melt(cor_matrix)

# Vẽ heatmap bằng ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

## price và square_feet tương quan dương 
# -> Khi diện tích tăng, giá cũng tăng theo
## price và price_per_sqft tương quan dương tương đối mạnh
# -> Khi giá tổng tăng, giá mỗi feet vuông cũng có xu hướng tăng.
## bedrooms và square_feet tương quan dương mạnh
# -> Những ngôi nhà có nhiều phòng ngủ thường có diện tích lớn hơn.
## bathrooms và bedrooms tương quan dương
# -> Những ngôi nhà có nhiều phòng ngủ thường có nhiều phòng tắm hơn.
## price_per_sqft và square_feet tương quan âm
#-> Khi diện tích tăng, giá mỗi feet vuông có xu hướng giảm. 
#   Điều này có thể do hiệu ứng quy mô lớn (giá mỗi feet vuông giảm khi tổng diện tích lớn).


## Tạo biến giả (dummy variables) cho cột cityname
dummy_vars <- model.matrix(~ cityname + 0, data = df3)

# Chuyển đổi kết quả thành data frame
dummy_df <- as.data.frame(dummy_vars)

# Loại bỏ cột intercept để tránh dummy variable trap
dummy_df <- dummy_df[, -1]

# Nối các biến giả vào dữ liệu gốc (df3)
df4 <- cbind(df3, dummy_df)

## Loại bỏ cột cityname sau khi đã tạo các biến giả
final_df <- subset(df4, select = -cityname)
final_df <- clean_names(final_df)

dim(final_df)

## Mối quan hệ giữa các biến sau bước tiền xử lý:
# Mối quan hệ giữa diện tích căn hộ và giá thuê
ggplot(final_df, aes(x = square_feet, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa diện tích căn hộ và giá thuê") + 
  xlab("Diện tích căn hộ (square feet)") + 
  ylab("Giá thuê (USD)")

# Mối quan hệ giữa số phòng ngủ và giá thuê
ggplot(final_df, aes(x = bedrooms, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa số phòng ngủ và giá thuê") + 
  xlab("Số phòng ngủ") + 
  ylab("Giá thuê (USD)") + 
  theme_minimal()

# Mối quan hệ giữa số phòng tắm và giá thuê
ggplot(final_df, aes(x = bathrooms, y = price)) + 
  geom_point(alpha = 0.5) + 
  ggtitle("Mối quan hệ giữa số phòng tắm và giá thuê") + 
  xlab("Số phòng tắm") + 
  ylab("Giá thuê (USD)") + 
  theme_minimal()

## Nhận xét:
# Thống qua các biểu đồ bên trên, ta có thể thấy rõ rằng các điểm dữ liệu ngoại lai đã được loại bỏ
# Dữ liệu thiếu trong các cột bathrooms, bedrooms, và cityname đã được xử lý hoặc loại bỏ để đảm bảo tính toàn vẹn và chính xác của dữ liệu.
# Dữ liệu hiện tại đã sẵn sàng cho bước xây dựng mô hình tiếp theo

# -------- Xây dựng mô hình hồi quy tuyến tính --------

# PLoan: trình bày từng mục xây dựng model, nhận xét về hiệu suất, nhận xét biểu đồ
# Hướng giải quyết, model thay đổi ntn => insights

set.seed(42) 
trainIndex <- createDataPartition(final_df$price, p = 0.7, list = FALSE)
train_data <- final_df[trainIndex, ]
test_data <- final_df[-trainIndex, ]

# Xây dựng mô hình hồi quy đơn giản
lm_model <- lm(price ~ ., data = train_data)

# Thống kê tổng hợp
summary(lm_model)

# Nhận xét chung: Bảng thống kê tổng hợp cho thấy các biến như bathrooms, 
##bedrooms, square_feet và price_per_sqft có ảnh hưởng mạnh và có ý nghĩa thống kê cao 
##đến giá nhà vì giá trị Pr(>|t|) của các features này rất nhỏ (đều dưới 0.001).   
##Các thành phố cũng có tác động tích cực hoặc tiêu cực đáng kể đối với giá nhà. 

# Dự đoán trên tập test
predictions <- predict(lm_model, newdata = test_data)

# Đánh giá model 
RMSE <- sqrt(mean((test_data$price - predictions)^2))
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
# Nhận xét:

## Hồi quy từng bước với cross validation
# Hàm predict.regsubsets() để tính giá trị tiên đoán Y dựa trên mô hình Mj 
# (kết quả từ regsubsets()) và dữ liệu trong fold thứ r.
predict.regsubsets <- function(object, newdata, id_model){
  form <- as.formula(object$call[[2]])
  x_mat <- model.matrix(form, newdata)
  coef_est <- coef(object, id = id_model)
  x_vars <- names(coef_est)
  res <- x_mat[, x_vars] %*% coef_est
  return(as.numeric(res))
}
# Chọn số fold là 10 
n_df <- nrow(final_df)
k <- 10
set.seed(21)
folds <- sample(rep(1:k, length = n_df))
# Tính 10-fold cross-validated error cho 181 mô hình con sử dụng phương pháp "forward"
cv_error_df_rj <- matrix(0, nrow = k, ncol = 181)
for(r in 1:k){
  df_train_r <- final_df[folds != r, ]
  df_test_r <- final_df[folds == r, ]
  out_subset_df_folds <- regsubsets(x = price ~ ., data = df_train_r,
                                    method = "forward", nvmax = 181, really.big = T)
  for(j in 1:181){
    pred_rj <- predict(out_subset_df_folds,
                       newdata = df_test_r, id_model = j)
    cv_error_df_rj[r, j] <- sqrt(mean((df_test_r$price - pred_rj)^2))
  }
}
cv_error_df <- colMeans(cv_error_df_rj)
# Biểu diễn kết quả 10-fold cross-validated error cho 181 mô hình con
ggplot(data = data.frame(x = c(1:181), y = cv_error_df),
       mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of predictors", y = "RMSE") +
  theme_bw()
# NHẬN XÉT BIỂU ĐỒ:

# Giá trị nhỏ nhất của 10-fold cross-validated error tương ứng với mô hình có số biến là 142
which.min(cv_error_df)

# Thực hiện regsubsets() và xác định mô hình con với 142 biến hồi quy
out_subset_df_2 <- regsubsets(x = price ~ ., data = final_df,
                              method = "forward", nvmax = 181)
# Xác định số lượng biến hồi quy tối ưu (ở đây giả định là 142 biến hồi quy)
optimal_model_id <- which.min(cv_error_df)  # Chọn mô hình có RMSE nhỏ nhất

# Lấy các hệ số của mô hình tối ưu
optimal_model_coefficients <- coef(out_subset_df_2, id = optimal_model_id)
optimal_model_coefficients

## Xây dựng model từ kết quả của cross validation
# Tạo công thức hồi quy tuyến tính từ các hệ số
selected_variables <- names(optimal_model_coefficients)[-1]  # Loại bỏ intercept
formula_str <- paste("price ~", paste(selected_variables, collapse = " + "))
formula_optimal <- as.formula(formula_str)

# Xây dựng mô hình hồi quy cuối cùng với các biến tối ưu
cv_model <- lm(formula_optimal, data = train_data)

# Xem thống kê tổng hợp của mô hình cuối cùng
summary(cv_model)
# NHẬN XÉT: 

# Dự đoán trên tập test
predictions <- predict(cv_model, newdata = test_data)

# Đánh giá mô hình bằng cách tính RMSE
RMSE <- sqrt(mean((test_data$price - predictions)^2))
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")

## Xây dựng mô hình hồi quy theo phương phấp hệ số co
x_house <- model.matrix(price ~ ., data = final_df)[, -1]
y_price <- final_df$price

lambda_grid <- 10^seq(from = 10, to = -2, length = 100)

set.seed(24)
out_cv_lasso <- cv.glmnet(x = x_house, y = y_price, alpha = 1,
                          type.measure = "mse", nfolds = 5,
                          family = "gaussian")
print(out_cv_lasso)

beta_lambda_lasso <- out_cv_lasso$lambda.min
out_lasso_md <- glmnet(x = x_house, y = y_price, alpha = 1,
                       lambda = lambda_grid, family = "gaussian")
predict(out_lasso_md, s = beta_lambda_lasso, type = "coefficients")

lm_model_lasso <- lm(price ~ bathrooms + square_feet + price_per_sqft + cityname_san_francisco + cityname_washington, data = train_data)
summary(lm_model_lasso)

#NHẬN XÉT:

# -------- Chuẩn đoán thặng dư của mô hình ------------
## Biểu đồ thặng dư của mô hình 
ggplot(data = cv_model, mapping = aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  theme_bw()

#NHẬN XÉT!

## Kiểm tra tính tuyến tính từng phần
terms_df <- predict(cv_model, type = "terms")
head(terms_df)

partial_resid_df <- residuals(cv_model, type = "partial")
head(partial_resid_df)
# Ta sẽ đánh giá tính tuyến tính của thành phần square_feet trong mô hình
data_part_resid_square_feet_df <- tibble(
  square_feet = train_data$square_feet,
  terms_square_feet = terms_df[, "square_feet"],
  partial_resid_square_feet = partial_resid_df[, "square_feet"]
)

ggplot(data_part_resid_square_feet_df, mapping = aes(square_feet, partial_resid_square_feet)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed",
              color = "forestgreen") +
  geom_line(aes(x = square_feet, y = terms_square_feet), color = "blue") +
  labs(x = "Square feet", y = "Partial Residuals") +
  theme_bw()
#NHẬN XÉT!

## Kiểm tra tính đồng nhất phương sai
ggplot(cv_model, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "loess", na.rm = TRUE, se = FALSE) +
  labs(x = "Fitted Values", y = expression(sqrt("|Standardized residuals|"))) +
  theme_bw()

## Kiểm tra điểm ngoại lai trong mô hình
ggplot(cv_model, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd)) +
  xlab("Leverage") + ylab("Standardized Residuals") +
  scale_size_continuous("Cook's Distance", range = c(1, 6)) +
  theme_bw() +
  theme(legend.position = "bottom")

std_resid_df <- rstandard(cv_model)
hat_values_df <- hatvalues(cv_model)
cooks_D_df <- cooks.distance(cv_model)

data_cooks_df <- tibble(id_point = 1:nrow(train_data),
                        rstand = std_resid_df, hats = hat_values_df,
                        cooks = cooks_D_df, sales = train_data$price)
data_cooks_df |> arrange(desc(cooks))

data_cooks_df_sorted <- data_cooks_df |> arrange(desc(cooks))
outliers <- data_cooks_df_sorted[data_cooks_df_sorted$cooks > 4/5044, ]
print(outliers)

#---------- Mở rộng mô hình hồi quy --------
# Dựa vào kết quả từ biểu đồ cooks, ta sẽ loại bỏ các điểm ngoại lai từ train_data
# Thêm cột chỉ số (index) vào train_data
train_data$index <- seq_len(nrow(train_data))
# Kiểm tra lại train_data sau khi thêm cột chỉ số
head(train_data)
# Loại bỏ các điểm ngoại lai từ train_data
train_data_clean <- train_data[!train_data$index %in% outliers$id_point, ]
# Xóa cột index 
train_data_clean <- train_data_clean[, -ncol(train_data_clean)]
# Xây dựng model dựa trên tập train đã loại bỏ outliers
lm_model_clean <- lm(formula_optimal, data = train_data_clean)
summary(lm_model_clean)
#NHẬN XÉT

## Mở rộng thành phần square_feet lên bậc 2 để ước lượng model
lm_model_poly <- lm(price ~ poly(square_feet, 2) + ., data = train_data_clean)
summary(lm_model_poly)
#NHẬN XÉT

#Kết luận:



