devtools::install_github("openwashdata/ds4owdretention")


get_course_participation_data = function(){
# Get the data
course_participation <- ds4owdretention::ds4owdretention

fs::dir_create("data", "course_participation")
write_csv(course_participation, "data/course_participation/course_participation.csv")
}
