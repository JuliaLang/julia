using Base.I18n

@test locale()==""
locale("en_US")
@test locale()=="en_US"
