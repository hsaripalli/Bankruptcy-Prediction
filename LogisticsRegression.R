logit <- glm(bk~eps+liquidity+profitability+productivity+leverage_ratio+asset_turnover+operational_margin
             +return_on_equity+market_book_ratio+assets_growth+sales_growth+employee_growth,
             data = bank,family = "binomial")
summary(logit)

predict_glm <- predict(logit,test,type="response")
predict_glm

predict_glm_class <- as.factor(ifelse(predict_glm > 0.5, 1,0))
confusionMatrix(predict_glm_class, reference = as.factor(test$bk))
