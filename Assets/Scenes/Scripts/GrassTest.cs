using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GrassTest : MonoBehaviour
{
    public GameObject character;

    // Start is called before the first frame update
    void Start()
    {
        Debug.Log("ENTRORETFZRFJGBNRSDJGBLSIREBG");
        character = GameObject.FindGameObjectWithTag("Player");
    }

    // Update is called once per frame
    void Update()
    {
        if(Vector3.Distance(transform.position, character.transform.position) < 5)
        {
            
            gameObject.SetActive(false);
        }
    }
}
