<?php
//get the form values
$username = $_POST['login_username'];
$userpass = $_POST['login_password'];


session_start();
$_SESSION['username'] = $username;
$_SESSION['userpass'] = $userpass;
header('Location: todo.php');
exit();