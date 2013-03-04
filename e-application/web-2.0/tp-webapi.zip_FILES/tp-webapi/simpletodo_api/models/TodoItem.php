<?php
class TodoItem
{
	public $todo_id;
	public $title;
	public $description;
	public $due_date;
	public $is_done;
	
	public static function getAllItems($username, $userpass)
	{
		self::_checkIfUserExists($username, $userpass);
		$userhash = sha1("{$username}_{$userpass}");
		$todo_items = array();
		foreach( new DirectoryIterator(DATA_PATH."/{$userhash}") as $file_info ) {
			// A terminer
		}
		
		return $todo_items;
	}
	
	public static function getItem($todo_id, $username, $userpass)
	{
		self::_checkIfUserExists($username, $userpass);
		$userhash = sha1("{$username}_{$userpass}");
		
		if( file_exists(DATA_PATH."/{$userhash}/{$todo_id}.txt") === false ) {
			throw new Exception('Todo ID is invalid');
		}
		
		$todo_item_serialized = file_get_contents(DATA_PATH."/{$userhash}/{$todo_id}.txt");
		
		$todo_record;
		
		// A terminer
		
		return $todo_record;
	}
	
	public function delete($username, $userpass)
	{
		self::_checkIfUserExists($username, $userpass);
		$userhash = sha1("{$username}_{$userpass}");
		
		if( file_exists(DATA_PATH."/{$userhash}/{$this->todo_id}.txt") === false ) {
			throw new Exception('Todo ID does not exist!');
		}
		
		// A terminer
		
		return true;
	}
	
	public function save($username, $userpass)
	{
		$userhash = sha1("{$username}_{$userpass}");
		if( is_dir(DATA_PATH."/{$userhash}") === false ) {
			mkdir(DATA_PATH."/{$userhash}");
		}
		
		//if the $todo_id isn't set yet, it means we need to create a new todo item
		if( is_null($this->todo_id) || !is_numeric($this->todo_id) ) {
			//the todo id is the current time
			$this->todo_id = time();
		}
		
		$todo_item_array;
		
		// A terminer
		
		//return the array version
		return $todo_item_array;
	}
	
	public function toArray()
	{
		//return an array version of the todo item
		return array(
			'todo_id' => $this->todo_id,
			'title' => $this->title,
			'description' => $this->description,
			'due_date' => $this->due_date,
			'is_done' => $this->is_done
		);
	}
	
	private static function _checkIfUserExists($username, $userpass)
	{
		$userhash = sha1("{$username}_{$userpass}");
		if( is_dir(DATA_PATH."/{$userhash}") === false ) {
			throw new Exception('Username  or Password is invalid');
		}
		return true;
	}
}