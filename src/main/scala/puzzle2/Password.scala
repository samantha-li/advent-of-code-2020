package puzzle2

case class Password(policy: Policy, value: String) {
  def valid: Boolean = {
    policy.validate(value)
  }
}