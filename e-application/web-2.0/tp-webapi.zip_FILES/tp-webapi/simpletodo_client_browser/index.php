<!DOCTYPE html>
<html>
<head>
	<title>SimpleTODO</title>
	
	<link rel="stylesheet" href="css/reset.css" type="text/css" />
	<link rel="stylesheet" href="css/bootstrap.min.css" type="text/css" />
	
	<script src="js/jquery.min.js"></script>
	<script src="js/jquery-ui-1.8.16.custom.min.js"></script>
	
	<style>
	body {
		padding-top: 40px;
	}
	#main {
		margin-top: 80px;
		text-align: center;
	}
	</style>
</head>
<body>
	<div class="topbar">
		<div class="fill">
			<div class="container">
				<a class="brand" href="index.php">SimpleTODO</a>
			</div>
		</div>
	</div>
	<div id="main" class="container">
		<form class="form-stacked" method="POST" action="login.php">
			<div class="row">
				<div class="span5 offset5">
					<label for="login_username">Username:</label>
					<input type="text" id="login_username" name="login_username" placeholder="username" />
				
					<label for="login_password">Password:</label>
					<input type="password" id="login_password" name="login_password" placeholder="password" />
					
				</div>
			</div>
			<div class="actions">
				<button type="submit" name="login_submit" class="btn primary large">Login or Register</button>
			</div>
		</form>
	</div>
</body>
</html>