obj = JSON.parse("{\"\U0001d712\":\"\\ud835\\udf12\"}")
@test(obj["𝜒"] == "𝜒")
