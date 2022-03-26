library(openxlsx)
library(dplyr)
library(tidyr)
library(caret)
library(shiny)
library(DT)
library(shinythemes)

#ชุดข้อมูลสำหรับพัฒนาโมเดลทำนายถูกจัดกระทำให้อยู่ในรูปแบบเดียวกันผ่าน Excel เรียบร้อยแล้ว
train <- read.xlsx("https://github.com/TeerawitK/Midterm-2758688/raw/main/train.xlsx")
test <- read.xlsx("https://github.com/TeerawitK/Midterm-2758688/raw/main/test.xlsx")

#ชุดข้อมูลสำหรับติดตามคะแนนซึ่งตัดมาเฉพาะคะแนนก่อนปลายภาค
score632 <- read.xlsx("https://github.com/TeerawitK/Midterm-2758688/raw/main/score632.xlsx")

#จัดกระทำข้อมูลให้อยู่ในรูปแบบที่ง่ายขึ้น
#train
train$att[train$att != 5] <- 0
train$att[train$att == 5] <- 1
table(train$att)

train$hw[train$hw != 5] <- 0
train$hw[train$hw == 5] <- 1
table(train$hw)

train$crt[train$crt == 0] <- 0
train$crt[train$crt != 0] <- 1
table(train$crt)

train$pps[is.na(train$pps) == T] <- 0
table(train$pps)

train$grade[train$grade == "I"] <- "F"
train$grade <- factor(train$grade, levels = c("A","B+","B","C+","C","D+","D","F"),
                    labels = c("A","B+","B","C+","C","D+","D","F"))
table(train$grade)

glimpse(train)

#test
test$att[test$att != 5] <- 0
test$att[test$att == 5] <- 1
table(test$att)

test$hw[test$hw != 5] <- 0
test$hw[test$hw == 5] <- 1
table(test$hw)

test$crt[test$crt == 0] <- 0
test$crt[test$crt != 0] <- 1
table(test$crt)

test$pps <- as.numeric(test$pps)
test$pps[is.na(test$pps) == T] <- 0
table(test$pps)

test$grade <- factor(test$grade, levels = c("A","B+","B","C+","C","D+","D","F"),
                     labels = c("A","B+","B","C+","C","D+","D","F"))
table(test$grade)

glimpse(test)


#Model
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

#ใช้คะแนนรวมเป็นตัวแปรตาม
#Linear Regression
fit.lm <- train(sum ~ midterm+att+hw+crt+pps, data = train,
                method = "lm",
                trControl = control)
pred.lm <- predict(fit.lm,test)
R2(pred.lm,test$sum)

#Elastic-net พบว่าประสิทธิภาพไม่แตกต่างจาก Linear Regression
grid.enet <- expand.grid(alpha = seq(0,1,0.1), lambda = seq(0,0.1,0.05))
fit.enet <- train(sum ~ midterm+att+hw+crt+pps, data = train,
                  method = "glmnet",
                  trControl = control,
                  tuneGrid = grid.enet)
pred.enet <- predict(fit.enet,test)
R2(pred.enet,test$sum)

#Knn พบว่าประสิทธิภาพต่ำกว่า Linear Regression
fit.knn <- train(sum ~ midterm+att+hw+crt+pps, data = train,
                method = "knn",
                trControl = control,
                tuneLength = 5)
pred.knn <- predict(fit.knn,test)
R2(pred.knn,test$sum)

#Random forest พบว่าประสิทธิภาพต่ำกว่า Linear Regression
fit.rf <- train(sum ~ midterm+att+hw+crt+pps, data = train,
                method = "rf",
                trControl = control,
                tuneLength = 4)
pred.rf <- predict(fit.rf,test)
R2(pred.rf,test$sum)

#ใช้ผลการเรียนเป็นตัวแปรตาม พบว่าโมเดลประสิทธิภาพต่ำกว่าใช้คะแนนรวมเป็นตัวแปรตาม
fit.knn2 <- train(grade ~ midterm+att+hw+crt+pps, data = train,
                  method = "knn",
                  trControl = control)

fit.rf2 <- train(grade ~ midterm+att+hw+crt+pps, data = train,
                 method = "rf",
                 trControl = control,
                 tuneLength = 4)

#โมเดลที่มีประสิทธิภาพดีที่สุดคือ Regression โดยมีคะแนนรวมเป็นตัวแปรตาม

#shiny
ui <- fluidPage(
  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Kanit:wght@400&display=swap');
      body {font-family: 'Kanit', sans-serif;}")),
        h1("ระบบให้คำแนะนำในการเรียนวิชาการวิจัยและสถิติเพื่อพัฒนาการเรียนรู้",
           style = "font-size:30px; color:#EE5736;"),
        h4("ภาคการศึกษาปลาย ปีการศึกษา 2563",
           style = "color:#1F618D"),
        theme = shinytheme('lumen'),
        tabsetPanel(
          tabPanel("ติดตามการเข้าเรียนและการส่งงาน",
                   h4("นิสิตสามารถติดตามการเข้าเรียนและการส่งงานของตนเองได้โดยค้นหาจากเลขประจำตัวนิสิต"),
                   DTOutput("mytable")
                   ),
                   
          tabPanel("ระบบให้คำแนะนำในการเรียน",
                  h5("ระบบให้คำแนะนำในการเรียนถูกพัฒนาขึ้นจากข้อมูลของนิสิตในภาคการศึกษาที่ผ่านมา
                      โดยระบบจะให้คำแนะนำและทำนายผลการเรียนของนิสิตจากข้อมูลการเข้าเรียน การส่งการบ้าน คะแนนงานโครงร่างวิจัย และคะแนนสอบกลางภาค
                      โมเดลทำนายผลการเรียนนี้เป็นโมเดล linear regression ซึ่งจากการวัดประสิทธิภาพการทำนายในข้อมูลชุดทดสอบพบว่ามีความแม่นยำ 86%"),
                  h4("ให้นิสิตกรอกข้อมูลของตนเองลงในแบบสอบถามต่อไปนี้แล้วกด Submit",
                     style = "color:#EE5736;"),
                fluidRow(
                  column(4,radioButtons(inputId = "att",
                                   choices = c("เข้าเรียนครบทุกครั้ง","ไม่ได้เข้าเรียนทุกครั้ง"),
                                   label = c("นิสิตเข้าเรียนครบทุกครั้งหรือไม่"))
                         ),
                  column(4,radioButtons(inputId = "hw",
                               choices = c("ส่งครบแล้ว","ยังส่งไม่ครบ"),
                               label = c("นิสิตส่งการบ้านครบหรือไม่"))
                         ),
                  column(4,radioButtons(inputId = "crt",
                               choices = c("ส่งแล้ว","ยังไม่ได้ส่ง"),
                               label = c("นิสิตได้ส่งงานวิพากษ์การวิจัยหรือไม่"))
                         )),
                fluidRow(
                  column(4,numericInput(inputId = "pps",
                               label = c("นิสิตได้คะแนนโครงร่างวิจัยกี่คะแนน"),
                               value = 0, min = 0, max = 10)),
                  column(4,numericInput(inputId = "midterm",
                               label = c("นิสิตได้คะแนนสอบกลางภาคกี่คะแนน"),
                               value = 0, min = 0, max = 25)),
                  column(4, br(), actionButton(inputId = "submit", 
                                               label = c("Submit")))
                        ),
                h5("หมายเหตุ: หากนิสิตแก้ไขข้อมูลในแบบสอบถาม ให้นิสิตกด Submit ใหม่ทุกครั้งเพื่อให้ระบบอัพเดทผลการทำนาย"),
                br(),
                h2(textOutput("title"),style = "color:#EE5736;"),
                h4(textOutput("feedback1"),style = "color:#1F618D;"),
                h4(textOutput("feedback2"),style = "color:#1F618D;"),
                h4(textOutput("value"),style = "color:#1F618D;"),
                br(),br(),
                textOutput("ps")
                 )
        )  
)

server <- function(input, output)
{
  output$mytable <- renderDT({datatable(score632)})
  a <- reactiveValues(result = "NULL")
  observeEvent(input$submit, {
    values = data.frame(att = ifelse(input$att == "เข้าเรียนครบทุกครั้ง",
                                      1,0),     
                        hw = ifelse(input$hw == "ส่งครบแล้ว",
                                    1,0), 
                        crt = ifelse(input$crt == "ส่งแล้ว",
                                     1,0), 
                        pps = input$pps,
                        midterm = input$midterm)
    a$result <-  round(predict(fit.lm, values), 
                       digits = 0)
  
    pred.grade <- ifelse(a$result>=85,"A",
                  ifelse(a$result<85&a$result>=80,"B+",
                  ifelse(a$result<80&a$result>=75,"B",
                  ifelse(a$result<75&a$result>=70,"C+",
                  ifelse(a$result<70&a$result>=65,"C",
                  ifelse(a$result<65&a$result>=60,"D+",
                  ifelse(a$result<60&a$result>=50,"D",
                  "F")))))))
    
    output$title <- renderText({"คำแนะนำในการเรียนและผลการทำนาย"})
    output$value <- renderText({
                      paste("ผลการทำนายพบว่านิสิตมีโอกาสที่จะได้ผลการเรียนเป็น ",
                            pred.grade)
                              })
    output$feedback1 <- renderText({
      ifelse(input$att == "เข้าเรียนครบทุกครั้ง"&
               input$hw == "ส่งครบแล้ว"&
               input$crt == "ส่งแล้ว"&
               input$pps != 0,
             "นิสิตมีความรับผิดชอบในการเข้าเรียนและการส่งงานดีมาก ในครึ่งหลังก็ขอให้นิสิตมีความรับผิดชอบที่ดีแบบนี้ต่อไป",
             ifelse(input$att != "เข้าเรียนครบทุกครั้ง"&
                      input$hw == "ส่งครบแล้ว"&
                      input$crt == "ส่งแล้ว"&
                      input$pps != 0,
                    "นิสิตมีความรับผิดชอบในการทำงานที่ได้รับมอบหมาย แต่นิสิตควรเข้าเรียนให้ครบทุกครั้งเนื่องจากมีคะแนนในส่วนของการเข้าเรียนด้วย",
                    ifelse(input$att == "เข้าเรียนครบทุกครั้ง"&
                             (input$hw != "ส่งครบแล้ว"|
                             input$crt != "ส่งแล้ว"|
                             input$pps == 0),
                           "นิสิตมีความรับผิดชอบในการเข้าเรียน แต่นิสิตควรติดตามส่งงานที่ยังไม่ได้ส่งให้เรียบร้อยเพื่อที่จะได้มีคะแนนหรือผลการเรียนที่ดีขึ้น",
                           "นิสิคควรเข้าเรียนและติดตามส่งงานให้เรียบร้อย มิเช่นนั้นผลการเรียนของนิสิตอาจจะออกมาไม่ดีเท่าที่ควร")))
    })
    output$feedback2 <- renderText({
      ifelse(input$midterm >= 18,
             "คะแนนสอบกลางภาคนิสิตทำได้ดี หากปลายภาคนิสิตตั้งใจทบทวนบทเรียนเหมือนในครั้งนี้ คะแนนสอบก็จะออกมามาดีเช่นกัน",
             ifelse(input$midterm >= 15 & input$midterm < 20,
                    "คะแนนสอบกลางภาคนิสิตทำได้ในระดับปานกลาง ก่อนสอบปลายภาคนิสิตควรทบทวนบทเรียนให้มากขึ้น",
                    ifelse(input$midterm >= 10 & input$midterm < 15,
                           "คะแนนสอบกลางภาคนิสิตทำได้ค่อนข้างน้อย ก่อนสอบปลายภาคนิสิตควรหาเวลาอ่านหนังสือทบทวนบทเรียน หรือทบทวนจากการบ้าน เพื่อที่จะได้สามารถทำข้อสอบได้มากขึ้น",
                           "คะแนนสอบกลางภาคนิสิตทำได้น้อยมาก หากมีบทเรียนไหนที่ไม่เข้าใจนิสิตสามารถถามอาจารย์หรือเพื่อนร่วมชั้นเรียนได้ เพื่อที่จะได้ติดตามเนื้อหาได้ทัน และสามารถทบทวนเนื้อหาเพื่อนำไปใช้สอบปลายภาคได้"
                           )))
    })
    output$ps <- renderText({"หมายเหตุ: ผลการทำนายข้างต้นเป็นเพียงการคาดการณ์เท่านั้น อนาคตอาจมีการเปลี่ยนแปลง
                              นิสิตควรใช้วิจารณญาณในการตัดสินใจ
                              อย่างไรก็ตามถ้าปลายภาคนิสิตเข้าเรียนสม่ำเสมอ 
                              ส่งงานครบตามที่ได้รับมอบหมาย และอ่านหนังสือทบทวนก่อนสอบปลายภาค
                              นิสิตก็จะสามารถผ่านวิชานี้ไปได้อย่างแน่นอน"})
    })
}

shinyApp(ui=ui, server=server)
